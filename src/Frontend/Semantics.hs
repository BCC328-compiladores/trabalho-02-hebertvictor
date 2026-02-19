{-  -------------------------------------
    @file       src/Frontend/Semantics.hs
    @details    Perform semantical analysis on the program.
-}

{-# LANGUAGE InstanceSigs #-} -- for which doesn't allow by default...

module Frontend.Semantics ( sl_verify,
                            sl_create_st,
                            
                            -- data types
                            SymbolTable,
                            GenericsMap
                            ) where

import Frontend.IR
import Frontend.Token
import Frontend.Parser
import Frontend.Error
import Frontend.Value
import Frontend.Pretty

import Data.Map ( Map )
import Data.List ( elemIndex, nub )
import Data.Either
import Data.Maybe ( isNothing, fromJust )
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad ( mapM, mapM_, zipWithM_ )


-----------------
-- SymbolTable --
-----------------

-- id <-> function / structure.
type SymbolTable = Map Identifier IR_Statement


-- just for the convenience of a default function statement...
null_function :: IR_Statement
null_function = FuncDef {
    symbol_name = "@undef_function",
    function_rtype = TypeVoid,
    function_parameters = [],
    function_gtypes = [],
    function_body = [],
    symbol_pos = SrcPos (-1, -1)
} 


print_function :: IR_Statement
print_function = Data.Either.fromRight null_function $ parsed where
    parsed = parse_sl_stat "forall a . func print(arg: a) : int { @print<<arg>>; return @rc; }"


scan_function :: IR_Statement
scan_function = Data.Either.fromRight null_function $ parsed where
    parsed = parse_sl_stat "forall a . func scan(arg: a) : int { @scan<<arg>>; return @rc; }"


st_insert :: IR_Statement -> SymbolTable -> SymbolTable
st_insert statement = Map.insert (symbol_name statement) statement


-- já vem com as funções de IO scan e print...
base_st :: SymbolTable
base_st = st_insert scan_function $ st_insert print_function Map.empty


-- Creates the ST.
-- Possibly multiple symbol mapping.
create_multiple_st :: IR_Program -> Map Identifier [IR_Statement]
create_multiple_st (Program sss) = __create_st sss base_multiple_st where
    -- basically a fold.
    __create_st [] st = st
    __create_st (s:ss) st = 
        case Map.lookup sname st of
            Nothing     -> __create_st ss (Map.insert sname [s] st)
            Just ss'    -> __create_st ss (Map.insert sname (s : ss') st)

            where 
                sname = symbol_name s

    base_multiple_st = (Map.insert "scan" [scan_function] . Map.insert "print" [print_function]) Map.empty


create_unique_st :: Map Identifier [IR_Statement] -> ([Error],  SymbolTable)
create_unique_st multiple_st = Map.foldrWithKey step ([], Map.empty) multiple_st
    where
        step key [x] (errors, st)   = (errors, Map.insert key x st)
        step key xs  (errors, st)   = (error_duplicate_symbol key (reverse xs) : errors, st)


error_duplicate_symbol :: Identifier -> [IR_Statement] -> Error
error_duplicate_symbol symbol_name ss = Error SemanticalError msg pos where
    msg = "Symbol " ++ show symbol_name ++ " defined multiple (" ++ show (length ss) ++ ") times"
    pos = symbol_pos $ ss !! 1 -- the second occurrence is the error position!


-- In case of error on the symbols, returns a non-empty list with the conflictant identifiers.
sl_create_st :: IR_Program -> Either Error SymbolTable
sl_create_st p = case create_unique_st $ create_multiple_st p of
    ([], st)    -> Right $ st
    (errors, _) -> Left $ MultipleErrors errors



---------------------------------
-- Semantical Analysis context --
---------------------------------

data VariableInfo = VariableInfo { -- state
    varstat_name        :: Identifier,
    varstat_type        :: IR_Type,
    varstat_use_count   :: Int,
    varstat_def_pos     :: SrcPos,

    -- ID reference.
    -- As of now, used for lambda lifting...
    varstat_ref         :: Identifier,

    varstat_level       :: Int
} deriving (Eq, Show)

varstat_to_vardecl :: VariableInfo -> IR_Var
varstat_to_vardecl (VariableInfo vname vtype _ _ _ _) = VarDecl vname vtype


type GenericsMap = Map Identifier IR_Type
type VariableMap = Map Identifier VariableInfo


variable_to_access :: [Identifier] -> [IR_VarAccess]
variable_to_access vars = ((\s -> VarAccess s VarAccessNothing) <$> vars)


descent_vm :: VariableMap -> VariableMap -> VariableMap
descent_vm base_vm other_vm = __descent (Map.toList other_vm) base_vm
  where
    __descent [] acc = acc

    __descent ((vname, vinfo_other):xs) acc =
        case Map.lookup vname acc of
            Nothing ->
                __descent xs acc
            
            Just (VariableInfo _ vtype use_base def_pos vref level_base) ->
                case vinfo_other of
                    VariableInfo _ _ use_other _ _ level_other ->
                        if level_other >= level_base then
                            __descent xs m

                        else do
                            __descent xs acc

                        where 
                            m = Map.insert vname (VariableInfo vname vtype novo_contador def_pos vref level_base) acc
                            novo_contador
                                | level_other > level_base  = use_other + use_base
                                | level_other == level_base = use_other

fc_next_level :: SemanticalContext ()
fc_next_level = do
    fc <- sc_get_fc

    let cl = fc_context_level fc
    sc_set_fc $ fc { fc_context_level = cl + 1 }

fc_previous_level :: SemanticalContext ()
fc_previous_level = do
    fc <- sc_get_fc

    let cl = fc_context_level fc
    sc_set_fc $ fc { fc_context_level = cl - 1 }


default_fc = FC null_function False 0 0

data FunctionContext = FC {
    fc_statement    :: IR_Statement,
    fc_has_return   :: Bool, -- TRACKS IF THE FUNCTION HAS A RETURN AT THE END!
    
    fc_lambda_count :: Int, -- NOT SUPPOSED TO BE HERE, BUT ANYWAYS...
    fc_context_level :: Int
} deriving (Eq, Show)


type CMap = Map Identifier [Identifier]

data SemanticalState = SemanticalState {
    ss_st       :: SymbolTable,
    ss_gm       :: GenericsMap,
    ss_vm       :: VariableMap, -- @TODO actually, this goes to function context?
    ss_fc       :: FunctionContext, -- @TODO rename into SCOPE?!
    ss_cmap     :: CMap,
    ss_src_pos  :: SrcPos
} deriving (Eq, Show)


newtype SemanticalContext v = SC {
    semantical_analysis_context_run :: SemanticalState -> (v, SemanticalState, [Error])
}


instance Functor SemanticalContext where
    fmap f (SC transition) = SC $ \state ->
        -- evaluating analysis transition.
        let (value, state', errors) = transition state
        in  (f value, state', errors)


instance Applicative SemanticalContext where
    pure :: a -> SemanticalContext a
    pure x = SC $ \state -> (x, state, [])

    (SC transition_f) <*> (SC transition_x) = SC $ \state -> 
        -- evaluating the function;
        let (value_f, state_f, errors_f) = transition_f state
        
        -- evaluating the value;
            (value_x, state_x, errors_x) = transition_x state_f

        -- compositing f with x and unifying the errors.
        in  (value_f value_x, state_x, errors_f ++ errors_x)


instance Monad SemanticalContext where
    -- alias
    return = pure

    (SC transition) >>= k = SC $ \state -> 
        -- executes the first action,
        let (v, state', errors) = transition state
        
        -- gets the seconds and then evaluates it,
            SC transition' = k v
            (v', state'', errors') = transition' state'

        -- and returns the final value, state, and the union of the errors.
        in  (v', state'', errors ++ errors')


-- os getters / setters de sempre...
sc_get_st :: SemanticalContext SymbolTable
sc_get_st = SC $ \state -> (ss_st state, state, [])

sc_get_gm :: SemanticalContext GenericsMap
sc_get_gm = SC $ \state -> (ss_gm state, state, [])

sc_get_vm :: SemanticalContext VariableMap
sc_get_vm = SC $ \state -> (ss_vm state, state, [])

sc_get_fc :: SemanticalContext FunctionContext
sc_get_fc = SC $ \state -> (ss_fc state, state, [])

sc_get_pos :: SemanticalContext SrcPos
sc_get_pos = SC $ \state -> (ss_src_pos state, state, [])

sc_get_cmap :: SemanticalContext CMap 
sc_get_cmap = SC $ \state -> (ss_cmap state, state, [])


sc_set_st :: SymbolTable -> SemanticalContext ()
sc_set_st st = SC $ \(SemanticalState _ gm vm fc cmap pos) -> ((), SemanticalState st gm vm fc cmap　pos, [])

sc_set_gm :: GenericsMap -> SemanticalContext ()
sc_set_gm gm = SC $ \(SemanticalState st _ vm fc cmap pos) -> ((), SemanticalState st gm vm fc cmap　pos, [])

sc_set_vm :: VariableMap -> SemanticalContext ()
sc_set_vm vm = SC $ \(SemanticalState st gm _ fc cmap pos) -> ((), SemanticalState st gm vm fc cmap　pos, [])

sc_set_fc :: FunctionContext -> SemanticalContext ()
sc_set_fc fc = SC $ \(SemanticalState st gm vm _ cmap pos) -> ((), SemanticalState st gm vm fc cmap pos, [])

sc_set_cmap :: CMap -> SemanticalContext ()
sc_set_cmap cmap = SC $ \(SemanticalState st gm vm fc _ pos) -> ((), SemanticalState st gm vm fc cmap pos, [])

sc_set_pos :: SrcPos -> SemanticalContext ()
sc_set_pos pos = SC $ \(SemanticalState st gm vm fc cmap _) -> ((), SemanticalState st gm vm fc cmap pos, [])


sc_raise :: String -> SemanticalContext ()
sc_raise error_msg = SC $ \state -> 
    ((), state, [Error SemanticalError error_msg (ss_src_pos state)])



-----------------------------
-- Semantical verification --
-----------------------------

-- Verifies the program; in case of success, returns the verified program.
sl_verify :: IR_Program -> Either Error (IR_Program, SymbolTable)
sl_verify p = do -- from either

    -- creating the symbol table.
    st <- sl_create_st p

    let (r, final_state, errs) = semantical_analysis_context_run (verify_program p) (SemanticalState st Map.empty Map.empty default_fc Map.empty (SrcPos (-1, -1)))

    case errs of
        []  -> Right $ (r, ss_st final_state)
        _   -> Left $ MultipleErrors errs


verify_program :: IR_Program -> SemanticalContext IR_Program
verify_program p@(Program statements) = do
    statements' <- mapM verify_statement statements

    -- extending the lambdas statements.
    st <- sc_get_st

    let lambdas = filter is_lambda (snd <$> Map.toList st) where
        is_lambda stmt = case stmt of       
            FuncDef sname _ _ _ _ _     -> length sname > 7 && (take 7 sname == "@LAMBDA") -- "@LAMBDA"
            StructDef _ _ _             -> False
    
    let statements'' = lambdas ++ statements'
    return $ Program statements''


verify_statement :: IR_Statement -> SemanticalContext IR_Statement
verify_statement f@(FuncDef sname rtype param gtypes body pos) = do
    verify_function f Map.empty

verify_statement s@(StructDef sname fields pos) = do
    -- 1 - structure can't have no field.
    -- 2 - each field should be verified.
    sc_set_pos pos

    case fields of 
        []  -> sc_raise $ "Structure " ++ show sname ++ " have no fields!"
        _   -> return $ ()

    fields' <- mapM (verify_field sname) fields
    let s' = (StructDef sname fields' pos)

    -- updating the symbol table.
    st <- sc_get_st
    sc_set_st $ Map.insert sname s' st

    return $ s'


verify_function :: IR_Statement -> VariableMap -> SemanticalContext IR_Statement
verify_function f@(FuncDef sname rtype param gtypes body pos) context_vm = do   
    -- 1 - well defined scope & symbol resolution;
    -- 2 - concordance with return expression and function's rtype;
    -- 3 - what more?
    --sc_raise $ ">>> VERIFY FUNCTION " ++ show sname

    -- saving older state...
    gm <- sc_get_gm
    vm <- sc_get_vm

    -- setting up state.
    sc_set_vm context_vm
    sc_set_pos pos
    
    fc <- sc_get_fc
    sc_set_fc $ FC f False (fc_lambda_count fc) (fc_context_level fc)
    fc_next_level

    mapM_ (\g -> load_generic g TypeVoid) gtypes -- loading generics.
    mapM_ load_variable param -- loading the variables into memory.

    -- verifying each command individually...
    commands' <- mapM verify_command body
    let commands'' = expand_command_list commands'

    -- checking for end-of-function state.
    verified_rtype <- verify_type rtype
    verify_function_final_state 

    -- resetting analysis state...
    sc_set_gm gm
    sc_set_vm vm

    sc_set_fc $ fc
    fc_previous_level

    let new_function = FuncDef sname verified_rtype param gtypes commands'' pos
    st <- sc_get_st
    sc_set_st $ st_insert new_function st

    return $ new_function


verify_function_final_state :: SemanticalContext ()
verify_function_final_state = do
    -- 1 - explicit return;
    -- 2 - variables are used;
    -- 3 - what more? 

    fc <- sc_get_fc
    let sname   = (symbol_name . fc_statement) fc
    let pos     = (symbol_pos . fc_statement) fc
    let rtype   = (function_rtype . fc_statement) fc
    
    sc_set_pos pos

    -- does this function is rtyped yet doesn't have an explicit return at the end?
    case rtype of
        TypeVoid    -> return $ () -- then a return isn't expected.
        _           -> do
            fc <- sc_get_fc
            if fc_has_return fc then return $ ()
            else do
                sc_raise $ "Function " ++ show sname ++ " have no explicit return but expects " ++ pretty_sl rtype

    _ <- verify_closure_final_state
    return $ ()
    -- @TODO: what more?


verify_closure_final_state :: SemanticalContext ()
verify_closure_final_state = do
    vm <- sc_get_vm

    -- at the end of the function, every variable should be used.
    Map.foldrWithKey verify_if_variable_is_accessed (return ()) vm
    where 
        verify_if_variable_is_accessed _ (VariableInfo vname vtype access_count def_pos vref level) acc = do
            _ <- acc
            sc_set_pos def_pos
            
            fc <- sc_get_fc
            
            if access_count < 1 && level == fc_context_level fc then do
                let sname   = (symbol_name . fc_statement) fc

                sc_raise $ "Variable " ++ show vname ++ " (" ++ pretty_sl vtype ++") is not being used on function " ++ show sname -- ++ " [" ++ show level ++ ", " ++ (show $ fc_context_level fc) ++ "]"

            else return $ ()


------------------------
-- Verifying commands --
------------------------

set_vref :: Identifier -> Identifier -> SemanticalContext ()
set_vref vname vref = do

    vm <- sc_get_vm
    case Map.lookup vname vm of
        Just (VariableInfo _ vtype use_count def_pos _ level) ->
            sc_set_vm $ Map.insert vname (VariableInfo vname vtype use_count def_pos vref level) vm

        _ -> return $ ()


-- recursive descent expanding what is (eliminating) `CmdList`.
expand_command_list :: [IR_LocatedCommand] -> [IR_LocatedCommand]
expand_command_list commands = __expand_command_list commands [] where
    __expand_command_list [] acc                                            = acc
    __expand_command_list ((LC (CmdList cmds) _) : xs) acc                  = __expand_command_list xs (acc ++ cmds)
    __expand_command_list ((LC (If exp cmds1 cmds2) pos) : xs) acc          = __expand_command_list xs (acc ++ [LC (If exp (expand_command_list cmds1) (expand_command_list cmds2)) pos])
    __expand_command_list ((LC (While exp cmds) pos) : xs) acc              = __expand_command_list xs (acc ++ [LC (While exp (expand_command_list cmds)) pos])
    __expand_command_list ((LC (For cmd1 exp2 exp3 cmds) pos) : xs) acc     = __expand_command_list xs (acc ++ [LC (For cmd1 exp2 exp3 (expand_command_list cmds)) pos])
    __expand_command_list (x:xs) acc                                        = __expand_command_list xs (acc ++ [x])


verify_command :: IR_LocatedCommand -> SemanticalContext IR_LocatedCommand
verify_command (LC (VarDef vdecl@(VarDecl vname vtype) exp) pos) = do

    sc_set_pos pos
    
    (exp', exp_type)    <- verify_expression exp
    vtype'              <- verify_type vtype

    exp_type <- verify_type exp_type

    if type_match vtype' exp_type then
        -- ok, expected type.
        return $ ()

    else do
        
        -- @TODO NYI, and the verification can improve a bit.
        -- But the idea being: if the expression is of concrete type and 
        -- and the type is generic, then the generic can be solved...
        if is_generic vtype' && (not . is_generic) exp_type then do
            return $ ()
        else do
            return $ ()

        case vtype' of
            TypeGeneric g   -> do
                -- gotta overload the generic.
                load_generic g exp_type

            _ -> sc_raise $ "Variable " ++ show vname ++ " (" ++ pretty_sl vtype' ++ ") is set to an expression that evaluates to " ++ pretty_sl exp_type  

    -- non-annotated type...
    vtype' <- if vtype' == TypeVoid && exp_type /= NoType then return $ exp_type
    else return $ vtype'

    -- loading the variable.
    load_variable (VarDecl vname vtype')

    case exp_type of 
        TypeGeneric _ -> error "VTMNC"
        _ -> return ()

    -- POR ENQUANTO TÁ BOM.
    case exp' of
        ExpFunctionReference sname -> do
            set_vref vname sname

        _ -> return $ ()

    return $ LC (VarDef (VarDecl vname vtype') exp') pos

verify_command (LC (Assignment varaccess exp) pos) = do
    sc_set_pos pos

    (varaccess', access_type) <- verify_access varaccess
    (exp', exp_type) <- verify_expression exp

    if type_match exp_type access_type then do
        -- then the types either couldn't be decided or they do match correctly.
        return $ ()

    else do
        -- then definitevely there is an error on the expressions type.
        sc_raise $ "Assigning expression of type " ++ pretty_sl exp_type ++ " to " ++ show (pretty_sl varaccess) ++ " (" ++ pretty_sl access_type ++ ")"

    return $ LC (Assignment varaccess' exp') pos

verify_command (LC (Return exp) pos) = do

    sc_set_pos pos

    fc <- sc_get_fc
    let rtype = function_rtype $ fc_statement fc
    verified_rtype <- verify_type rtype
    
    (exp', possible_type) <- verify_expression exp

    -- @TODO verify if in a complex command (control-flux / repetition)!
    if function_rtype_match verified_rtype possible_type then do
        return $ () -- OK
    else
        sc_raise $ "Function " ++ show (symbol_name $ fc_statement fc) ++ " expects type " ++ pretty_sl verified_rtype ++ "; yet, " ++ pretty_sl possible_type ++ " is returned"

    -- has return.
    fc <- sc_get_fc
    sc_set_fc $ FC (fc_statement fc) True (fc_lambda_count fc) (fc_context_level fc)

    -- @TODO check current rtype.
    return $ LC (Return exp') pos

verify_command cmd@(LC (If exp cmds1 cmds2) pos) = do
    sc_set_pos pos
    
    -- @TODO what to verify on control-flux?
    (exp', exp_type) <- verify_expression exp

    vm <- sc_get_vm
    fc_next_level

    cmds1' <- mapM verify_command cmds1
    verify_closure_final_state
    vm' <- sc_get_vm
    sc_set_vm $ descent_vm vm vm'

    cmds2' <- mapM verify_command cmds2
    verify_closure_final_state
    vm' <- sc_get_vm
    sc_set_vm $ descent_vm vm vm'

    fc_previous_level

    -- command-flux expression must be a boolean.
    if exp_type /= TypeBool then do
        sc_raise $ "Non-boolean evaluated expression in control-flux: " ++ pretty_sl exp_type
        return $ cmd

    else do
        
        case constant_eval exp' of
            Just (ValueBool True)   -> do
                let cmds1'' = expand_command_list cmds1'
                return $ LC (CmdList cmds1'') pos

            Just (ValueBool False)  -> do
                let cmds2'' = expand_command_list cmds2'
                return $ LC (CmdList cmds2'') pos

            _                       -> do
                -- no reduction possible.
                let cmds1'' = expand_command_list cmds1'
                let cmds2'' = expand_command_list cmds2'
                return $ LC (If exp' cmds1'' cmds2'') pos

verify_command cmd@(LC (While exp cmds) pos) = do
    sc_set_pos pos

    vm <- sc_get_vm

    (exp', exp_type) <- verify_expression exp

    fc_next_level
    
    cmds' <- mapM verify_command cmds 
    let cmds'' = expand_command_list cmds'

    vm' <- sc_get_vm
    sc_set_vm $ descent_vm vm vm'

    fc_previous_level

    -- command-flux expression must be a boolean.
    if exp_type /= TypeBool then do
        sc_raise $ "Non-boolean evaluated expression in while: " ++ pretty_sl exp_type
        return $ cmd

    else do
        case constant_eval exp' of
            Just (ValueBool False)  -> do
                return $ LC (CmdList cmds'') pos

            _                       -> do
                -- no reduction possible.
                return $ LC (While exp' cmds'') pos

verify_command (LC for@(For init_cmd exp it_cmd cmds) pos) = do
    sc_set_pos pos
    
    let extended_cmds = cmds ++ [it_cmd]
    
    case init_cmd of
        LC (CmdExpression ExpNothing) _ -> do
            -- in that case, there's no need adding an extra "nothing" command, isn't it?
            verify_command $ LC (While exp extended_cmds) pos

        x -> do
            y <- verify_command $ LC (CmdList [
                x,
                LC (While exp extended_cmds) pos
                ]) pos 
            
            case y of
                LC (CmdList cmds') pos -> 
                    return $ LC (CmdList (expand_command_list cmds')) pos

verify_command (LC (CmdExpression exp) pos) = do
    sc_set_pos pos

    -- as simple as that...
    (exp', _) <- verify_expression exp
    return $ LC (CmdExpression exp') pos

verify_command (LC (Print exp) pos) = do
    sc_set_pos pos

    -- same thing...
    (exp', _) <- verify_expression exp
    return $ LC (Print exp') pos

verify_command (LC (Scan exp) pos) = do
    sc_set_pos pos

    -- same...
    (exp', _) <- verify_expression exp
    return $ LC (Scan exp') pos

verify_command (LC (CmdList cmds) pos) = do
    cmds' <- mapM verify_command cmds
    return $ LC (CmdList cmds') pos




----------------------------------------
-- Verifying variable scope and etc. --
----------------------------------------

set_captures :: Identifier -> [Identifier] -> SemanticalContext ()
set_captures sname captures = do
    cmap <- sc_get_cmap

    case Map.lookup sname cmap of
        Just captures' -> 
            sc_set_cmap $ Map.insert sname (nub $ captures' ++ captures) cmap

        _ -> 
            sc_set_cmap $ Map.insert sname captures cmap


load_generic :: Identifier -> IR_Type -> SemanticalContext ()
load_generic g t = do
    gm <- sc_get_gm
    sc_set_gm (Map.insert g t gm)


-- Attempts loading a variable into the variable map, in a function context.
load_variable :: IR_Var -> SemanticalContext ()
load_variable (VarDecl vname vtype) = do
    
    vm <- sc_get_vm
    pos <- sc_get_pos
    fc <- sc_get_fc

    -- checking for shadowing...
    case Map.lookup vname vm of 
        Nothing -> do
            -- OK.
            sc_set_vm (Map.insert vname (VariableInfo vname vtype 0 pos "undefined" (fc_context_level fc)) vm)

        Just (VariableInfo _ t _ def_pos vref level) -> do
            cmap <- sc_get_cmap

            if level < fc_context_level fc then do
                -- OK.
                sc_set_vm (Map.insert vname (VariableInfo vname vtype 0 pos "undefined" (fc_context_level fc)) vm)
                
            else do
                u <- case Map.lookup (symbol_name $ fc_statement fc) cmap of
                    Just xs     -> return $ any (== vname) xs
                    _           -> return $ False

                if u then do
                    return $ ()

                else
                    sc_raise $ "Variable " ++ show vname ++ " (" ++ pretty_sl t ++ "), first defined at " ++ pretty_sl def_pos ++ ", is redefined with type " ++ pretty_sl vtype


function_type :: IR_Statement -> IR_Type
function_type (FuncDef _ rtype parameters _ _ _) = TypeFunction param_types rtype where
    param_types = (\(VarDecl _ t ) -> t) <$> parameters
function_type _ = NoType


variable_type :: Identifier -> SemanticalContext IR_Type
variable_type vname = do

    vm <- sc_get_vm
    case Map.lookup vname vm of
        Nothing                             -> return NoType
        Just (VariableInfo _ vtype _ _ _ _) -> return vtype


-- Structurally similar to `ic_pm_read`.
-- Verifies the variable access, returning the a new version and the associated type.
-- Performs `verify_type` internally.
-- Returning `TypeVoid` means it had failed to determine the type of the access. 
verify_access :: IR_VarAccess -> SemanticalContext (IR_VarAccess, IR_Type)
verify_access access@(VarAccess vname next_access) = do
    -- First of all, we try getting the variable from the map.
    vm <- sc_get_vm
    case Map.lookup vname vm of
        Nothing -> do
            -- is it possibly a function then?
            -- (functional behavior...)

            st <- sc_get_st
            case Map.lookup vname st of
                Just f      -> do 
                    return $ (access, function_type f)
                _           -> do
                    sc_raise $ "Variable " ++ show vname ++ " is not defined"
                    return $ (access, NoType)

        Just (VariableInfo _ vtype access_count def_pos vref level)  -> do
            -- is defined!
            
            -- variable is accessed one more time!
            sc_set_vm $ Map.insert vname (VariableInfo vname vtype (access_count + 1) def_pos vref level) vm

            (va, vt) <- __access_type vname next_access vtype
            return $ (access, vt)

verify_access access@_ = do
    sc_raise $ "Invalid variable access: " ++ pretty_sl access ++ show access
    return $ (access, NoType)

-- Retrieves the type of the variable access.
__access_type :: Identifier -> IR_VarAccess -> IR_Type -> SemanticalContext (IR_VarAccess, IR_Type)
__access_type _ VarAccessNothing t = do
    t' <- verify_type t
    return $ (VarAccessNothing, t')

__access_type root_vname access@(VarAccessIndex index_exp next_access) acc = do
    verified_acc <- verify_type acc

    -- @TODO CHECK FOR CONSTANT VALID INDICES BEFOREHAND!
    case verified_acc of
        TypeArray base_type exps -> do
            case exps of 
                (_:xs@(_:_))    -> __access_type root_vname next_access $ TypeArray base_type xs
                _               -> __access_type root_vname next_access $ base_type -- singleton or empty

        _ -> do
            sc_raise $ "Indexing non-array (" ++ pretty_sl verified_acc ++ ") at " ++ pretty_sl access ++ " on variable " ++ show root_vname
            return $ (access, NoType)

__access_type root_vname access@(VarAccess vname next_access) acc = do
    verified_acc <- verify_type acc

    case verified_acc of 
        TypeStruct sname    -> do
            -- then it is a struct (controversely, if not, verify_type verified it wrongly).
            st <- sc_get_st
            case Map.lookup sname st of 
                Just (StructDef _ fields pos) -> do
                    -- yes, it was a structure.
                    -- then we have to understand if the access is a valid field.
                    let field_names = (\(VarDecl name _) -> name) <$> fields
                    let field_types = (\(VarDecl _ vtype) -> vtype) <$> fields

                    case elemIndex vname field_names of
                        Just i  -> do
                            -- Ok, it was a field of the structure correctly.
                            -- continuing the search from the next.
                            __access_type root_vname next_access (field_types !! i)
                        _ -> do
                            sc_raise $ "Invalid field " ++ show vname ++ " access of structure " ++ pretty_sl sname ++ " on variable " ++ show root_vname 
                            return $ (access, NoType)

                _ -> error "BEHAVIORIAL ERROR" -- inconsistency

        TypeGeneric g       -> do
            -- then it is probably a generic type...
            gm <- sc_get_gm
            case Map.lookup g gm of
                Just TypeVoid -> do
                    -- A GENERIC NOT YET SOLVED!
                    sc_raise $ "NYI DON'T KNOW WHAT TO DO"
                    return $ (access, NoType)

                Just vtype  -> do
                    -- found the corresponding, valid type!
                    __access_type root_vname access vtype

                _ -> do
                    -- then the identifier is neither a structure nor a type.
                    sc_raise $ "NYI Invalid access 55"
                    return $ (access, NoType)

        TypeArray base_type exps -> do
            case next_access of -- look ahead...
                VarAccessNothing -> do
                    -- then it is accessing a hidden field on the array type.
                    -- currently, there's just the 'size' of it. 
                    if vname == "size" then do
                        return $ (access, TypeInt) -- definetively an int!

                    else do
                        sc_raise $ "Invalid " ++ show vname ++ " field access on array on variable " ++ show root_vname
                        return $ (access, NoType)

                _ -> do
                    -- THAT'S PRETTY WRONG!
                    sc_raise $ "Invalid array access on variable " ++ show root_vname
                    return $ (access, NoType)

        TypeFunction param_types rtype -> do
            sc_raise $ "Ksks can't access a field of a function; what would that mean?"
            return $ (access, NoType)

        _ -> do
            sc_raise $ "Invalid access of " ++ show (pretty_sl access) ++ " on type " ++ pretty_sl verified_acc ++ " on variable " ++ show root_vname 
            return $ (access, NoType)
        


--------------------------------
-- Verifying fields and types --
--------------------------------

type_generics :: IR_Type -> [Identifier]
type_generics base_t = reverse $ __type_generics base_t [] where
    __type_generics (TypeGeneric g) acc         = g : acc
    __type_generics (TypeArray t _) acc         = __type_generics t acc
    __type_generics (TypeFunction ts rtype) acc = __type_generics rtype [] ++ concatMap (\t -> __type_generics t []) ts ++ acc
    __type_generics _ acc                       = acc


verify_field :: Identifier -> IR_Var -> SemanticalContext IR_Var
verify_field sname v@(VarDecl vname vtype) = do
    -- 1 - fields cannot be declared with either void or generics.
    -- 2 - arrays specifiers have to have constant literals as an result.
    vtype' <- case vtype of
        TypeVoid                    -> do
            sc_raise $ "Field " ++ show vname ++ " declared as void on struct " ++ show sname
            return $ vtype

        array@(TypeArray t exps)    -> do
            
            let constants = (constant_eval . reduce_expression) <$> exps
            let invalid_indices = filter __is_invalid $ zip constants [1..] where 
                __is_invalid (Nothing, _) = True
                __is_invalid (Just v, _) = (not . is_valid_index) v
                x = (\(v, i) -> is_valid_index v)
            
            case invalid_indices of
                []  -> do
                    return $ TypeArray t $ (\(Just (ValueInt x)) -> ExpLitInteger x) <$> constants -- ok.
                _   -> do
                    sc_raise $ "Invalid indices for array field " ++ show vname ++ " of base type " ++ pretty_sl t
                    return $ array

        function@(TypeFunction _ _) -> do
            sc_raise $ "Field " ++ show vname ++ " declared as a function on struct " ++ show sname
            return $ function
        
        _                   -> do
            case type_generics vtype of
                []  -> return $ vtype
                gs  -> do
                    -- possible generics.
                    -- but they confuse with struct names.
                    -- so we verify for that.
                    st <- sc_get_st
                    let lookups = map (\g -> (g, Map.lookup g st)) gs
                    let pure_generics = map fst $ filter (\(g, m) -> isNothing m) lookups
                    
                    case pure_generics of
                        []  -> return $ () -- everything's alright.
                        _   -> sc_raise $ "Field " ++ show vname ++ " declared as of generics " ++ show pure_generics ++ " on struct " ++ show sname 

                    return $ vtype

    return $ (VarDecl vname vtype')


is_valid_index :: Value -> Bool
is_valid_index (ValueInt n)
    | n >= 1    = True
    | otherwise = False
is_valid_index _ = False


-- @TODO
-- oh boy. that's where the inference may take place...
-- for instance, verifies for just the simple generic to map it.
-- Converts loose generics TypeGeneric to TypeStruct, when possible.
verify_type :: IR_Type -> SemanticalContext IR_Type
verify_type t@(TypeGeneric g) = do
    gm <- sc_get_gm
    case Map.lookup g gm of
        Just TypeVoid -> 
            -- type not registered yet.
            return $ TypeVoid
        
        Nothing -> do
            -- worse yet: not even a generic.
            -- then it is probably a struct.
            
            st <- sc_get_st
            case Map.lookup g st of
                Just (StructDef _ _ _) -> do
                    -- yes, it was a structure.
                    -- here is where it is informed to the IR.
                    return $ TypeStruct g
                
                _ -> do
                    -- nevermind.
                    sc_raise $ "Unknown type " ++ show (pretty_sl t)
                    return $ NoType

        Just t -> return $ t

verify_type t@(TypeArray base_type exps) = do
    base_type' <- verify_type base_type
    return $ TypeArray base_type' exps

-- FALTA FAZER PRA FUNÇÃO TAMBÉM,

-- else, there's nothing to be done.        
verify_type t = return $ t 



---------------------------
-- Verifying expressions --
---------------------------

-- Verifies and type-checks the expression.
verify_expression :: IR_Expression -> SemanticalContext (IR_Expression, IR_Type)

verify_expression exp@(ExpFCall sname args) = do
    --sc_raise $ ">>> VERIFY_EXPRESSION TYPE CHECK FCALL "
    
    verified <- mapM verify_expression args
    let args' = fst <$> verified
    let types' = snd <$> verified

    -- is the function on the symbol table or not?
    st <- sc_get_st
    pos <- sc_get_pos
    case Map.lookup sname st of
        -- ok, it was a function already there...
        Just (FuncDef _ rtype param _ _ _)  -> do
            let exp' = ExpFCall sname args'
            let param_types = (\(VarDecl _ t) -> t) <$> param

            rtype' <- verify_type rtype

            verify_fcall sname types' param_types rtype'
            return $ (exp', rtype')

        Just (StructDef _ _ pos) -> do
            sc_raise $ "Calling struct " ++ show sname ++ " (defined at " ++ pretty_sl pos ++ ") as a function!"
            return $ (exp, NoType)
        
        _ -> do
            -- is it a variable?
            -- then it is probably a local function.
            vm <- sc_get_vm
            case Map.lookup sname vm of
                Just (VariableInfo _ (TypeFunction param_types rtype) _ _ vref vlevel) -> do
                    
                    -- creating dummy access...
                    verify_access (VarAccess sname VarAccessNothing)

                    rtype' <- verify_type rtype

                    cmap <- sc_get_cmap
                    case Map.lookup vref cmap of
                        Just captures -> do
                            capture_types <- mapM variable_type captures 
                            --sc_raise $ show (types' ++ capture_types) ++ " / " ++ show param_types ++ show "; args: " ++ pretty_sl args' ++ show "; types: " ++ pretty_sl types' ++ "; captures: " ++ pretty_sl captures
                            verify_fcall sname (types' ++ capture_types) param_types rtype'

                            let scoped_args = (\s -> ExpVariable (VarAccess s VarAccessNothing)) <$> captures
                            let exp' = ExpFCall sname (args' ++ scoped_args)
                            return $ (exp', rtype')

                        _ -> do
                            verify_fcall sname types' param_types rtype'
                            return $ (exp, rtype')

                y   -> do
                    -- then the function really doesn't exists whatsoever.
                    sc_raise $ "Function " ++ show sname ++ " is undefined"
                    -- sc_raise $ show y
                    return $ (exp, NoType)

-- literals.
verify_expression exp@(ExpNothing)          = return $ (ExpNothing, TypeVoid)
verify_expression exp@(ExpLitInteger _)     = return $ (exp, TypeInt)
verify_expression exp@(ExpLitFloating _)    = return $ (exp, TypeFloat)
verify_expression exp@(ExpLitBoolean _)     = return $ (exp, TypeBool)
verify_expression exp@(ExpLitString _)      = return $ (exp, TypeString)

-- bops.
verify_expression exp@(ExpSum e1 e2)        = verify_expression_closed_bop "+" e1 e2    ExpSum
verify_expression exp@(ExpSub e1 e2)        = verify_expression_closed_bop "-" e1 e2    ExpSub
verify_expression exp@(ExpMul e1 e2)        = verify_expression_closed_bop "*" e1 e2    ExpMul
verify_expression exp@(ExpDiv e1 e2)        = verify_expression_closed_bop "/" e1 e2    ExpDiv
verify_expression exp@(ExpIntDiv e1 e2)     = verify_expression_closed_bop "//" e1 e2   ExpIntDiv
verify_expression exp@(ExpMod e1 e2)        = verify_expression_closed_bop "%" e1 e2    ExpMod
verify_expression exp@(ExpPow e1 e2)        = verify_expression_closed_bop "^" e1 e2    ExpPow
verify_expression exp@(ExpNegative e)       = verify_expression e

verify_expression exp@(ExpAnd e1 e2)        = verify_expression_bool_bop ExpAnd e1 e2 exp
verify_expression exp@(ExpOr e1 e2)         = verify_expression_bool_bop ExpOr e1 e2 exp

verify_expression exp@(ExpEq e1 e2)         = verify_expression_relational ExpEq e1 e2  exp
verify_expression exp@(ExpNeq e1 e2)        = verify_expression_relational ExpNeq e1 e2 exp
verify_expression exp@(ExpGt e1 e2)         = verify_expression_relational ExpGt e1 e2  exp
verify_expression exp@(ExpGeq e1 e2)        = verify_expression_relational ExpGeq e1 e2 exp
verify_expression exp@(ExpLt e1 e2)         = verify_expression_relational ExpLt e1 e2  exp
verify_expression exp@(ExpLeq e1 e2)        = verify_expression_relational ExpLeq e1 e2 exp

verify_expression exp@(ExpLIncr e)          = __verify_incr_exp e ExpLIncr "Left increment"
verify_expression exp@(ExpRIncr e)          = __verify_incr_exp e ExpRIncr "Right increment"
verify_expression exp@(ExpLDecr e)          = __verify_incr_exp e ExpLDecr "Left decrement"
verify_expression exp@(ExpRDecr e)          = __verify_incr_exp e ExpRDecr "Right decrement"

verify_expression exp@(ExpVariable va)                  = do
    (access, vtype) <- verify_access va
    return $ (ExpVariable access, vtype)

verify_expression exp@(ExpStructInstance sname args)    = do
    verified_args_and_types <- mapM verify_expression args
    let verified_args = fst <$> verified_args_and_types
    return $ (ExpStructInstance sname verified_args, TypeStruct sname)

verify_expression exp@(ExpArrayInstancing args)         = do
    verified_args_and_types <- mapM verify_expression args
    let args' = fst <$> verified_args_and_types
    let arg_types = snd <$> verified_args_and_types

    let exp' = ExpArrayInstancing args'

    case args' of 
        []      -> do
            sc_raise $ "Null-array instancing. What about a no?"
            return $ (exp', NoType)

        (e:_)   -> do
            let t = arg_types !! 0
            if not (and $ (== t) <$> arg_types) then do
                sc_raise $ "Non-homogeneous array instancing: " ++ pretty_sl arg_types
                return $ (exp', NoType)
            else
                case t of
                    TypeArray b array_exps  -> return $ (exp', TypeArray b ([array_size_expr] ++ array_exps))
                    _                       -> return $ (exp', TypeArray t [array_size_expr])
                    where array_size_expr = ExpLitInteger (toInteger $ length args')

verify_expression exp@(ExpNew t)                        = do
    case t of
        TypeArray base_type exps ->
            -- ok
            case exps of 
                []   -> do
                    -- should invariably never happen...
                    sc_raise $ "Invalid array instancing2: " ++ pretty_sl exp
                    return $ (exp, NoType)

                _   -> do
                    verified_exps <- mapM verify_expression exps
                    let types = snd <$> verified_exps

                    if (and $ (== TypeInt) <$> types)  then do
                        -- ok
                        return $ (exp, t)

                    else do
                        -- @TODO more detailed msg?
                        sc_raise $ "Invalid array instancing: " ++ pretty_sl exp
                        return $ (exp, NoType)
        
        _ -> do
            sc_raise $ "Allocating non-array type " ++ pretty_sl t  ++ " is meaningless"
            return $ (exp, NoType)

verify_expression exp@(ExpFCall_Implicit fcall_exp args) = do
    let rexp = reduce_expression fcall_exp

    -- well, the expression MUST be a lambda...
    -- no questions asked.
    case rexp of
        ExpLambda rtype param captures cmds -> do
            
            verified_args <- mapM verify_expression args
            let args' = fst <$> verified_args
            let arg_types = snd <$> verified_args
            let param_types = (\(VarDecl _ t) -> t) <$> param

            rtype' <- verify_type rtype
            verify_fcall "lambda" arg_types param_types rtype'

            return $ (ExpFCall_Implicit rexp args', rtype')

        _ -> do
            sc_raise $ "Instant evaluation of non-lambda expression: " ++ pretty_sl exp
            return $ (exp, NoType)

verify_expression exp@(ExpLambda rtype vars captures _) = do
    -- lifting lambda...
    -- at that step, function verification is performed...
    sname <- lift_lambda exp

    -- then, the type is that of TypeFunction.
    rtype' <- verify_type rtype

    if is_type_invalid rtype' then do
        sc_raise $ "Invalid return type for lambda function."
        return $ (exp, NoType)
        
    else do

        -- taking the types of the parameters...
        var_types <- mapM verify_type ((\(VarDecl _ t) -> t) <$> vars)
        
        capture_accesses <- mapM verify_access $ variable_to_access captures
        let capture_types = snd <$> capture_accesses

        if any is_type_invalid var_types then do
            sc_raise $ "Invalid parameter type on lambda function..."
            return $ (exp, NoType)

        else do
            -- 
            return $ (ExpFunctionReference sname, TypeFunction (var_types ++ capture_types) rtype')

verify_expression exp@(ExpFunctionReference sname) = do
    st <- sc_get_st
    case Map.lookup sname st of
        Just (FuncDef _ rtype _ _ _ _) -> return $ (exp, rtype)
        Nothing -> do
            sc_raise $ "INTERNAL: INVALID LIFTED LAMBDA"
            return $ (exp, NoType)


__verify_incr_exp :: IR_Expression -> (IR_Expression -> IR_Expression) -> String -> SemanticalContext (IR_Expression, IR_Type)
__verify_incr_exp e constructor msg = do
    (e', e_type) <- verify_expression e

    if e_type /= TypeInt then do
        sc_raise $ msg ++ " on non-int eval. expression: " ++ pretty_sl e ++ " (" ++ pretty_sl e_type ++ ")"
        return $ (ExpNothing, NoType)

    else do
        return (constructor e', TypeInt)


verify_expression_closed_bop :: String -> IR_Expression -> IR_Expression -> (IR_Expression -> IR_Expression -> IR_Expression) -> SemanticalContext (IR_Expression, IR_Type)
verify_expression_closed_bop bop_str e1 e2 constructor = do
    (e1', t1) <- verify_expression (reduce_expression e1)
    (e2', t2) <- verify_expression (reduce_expression e2)
    let exp = reduce_expression $ constructor e1' e2'

    if type_match t1 t2 then do
        -- prioritizing the type which is not TypeVoid.
        if t1 == TypeVoid then  return $ (exp, t2)
        else                    return $ (exp, t1)
    else do
        sc_raise $ "Invalid expression result of " ++ pretty_sl t1 ++ " " ++ bop_str ++ " " ++ pretty_sl t2
        return $ (exp, NoType)


verify_expression_bool_bop constructor e1 e2 exp = do 

    (e1', t1) <- verify_expression (reduce_expression e1)
    (e2', t2) <- verify_expression (reduce_expression e2)
    let exp' = reduce_expression $ constructor e1' e2'

    case t1 of
        TypeBool -> do
            case t2 of 
                TypeBool -> return $ (exp', TypeBool)
                _ ->  do
                    sc_raise $ "Non-boolean on second term of expression " ++ pretty_sl exp
                    return $ (exp', NoType)

        _ -> do
            sc_raise $ "Non-boolean on first term of expression " ++ pretty_sl exp
            return $ (exp', NoType)


verify_expression_relational constructor e1 e2 exp = do
    (e1', t1) <- verify_expression (reduce_expression e1)
    (e2', t2) <- verify_expression (reduce_expression e2)
    let exp' = reduce_expression $ constructor e1' e2'

    if type_match t1 t2 then do
        return $ (exp', TypeBool)
    else do
        sc_raise $ "Relational expression " ++ pretty_sl exp ++ " of types (" ++ pretty_sl t1 ++ ", " ++ pretty_sl t2 ++ ")"
        return $ (exp, NoType)



verify_fcall :: Identifier -> [IR_Type] -> [IR_Type] -> IR_Type -> SemanticalContext IR_Type
verify_fcall sname types param_types rtype = do

    if  length param_types /= length types then do
        sc_raise $ "Calling function " ++ show sname ++ " with wrong number of arguments (" ++ show (length types) ++ " out of " ++ show (length param_types) ++ ")"
    else return $ ()
    
    -- independently of the # of args...
    zipWithM_ (__verify_parameter sname) param_types (zip types [1..])

    return $ rtype

__verify_parameter :: Identifier -> IR_Type -> (IR_Type, Int) -> SemanticalContext ()
__verify_parameter sname param_type (exp_type, n) = do
    unsolved <- unsolved_type param_type
    
    if type_match exp_type param_type || unsolved then do
        -- OK.
        return $ ()

    else do 
        sc_raise $ "Invalid argument #" ++ show n ++ " on function " ++ show sname ++ " call: expected " ++ pretty_sl param_type ++ ", yet, argument is " ++ pretty_sl exp_type


-- Tell if two tipes are semantically compatible.
-- That is, if they're non-strictly equal or one of them is of TypeVoid.
type_match :: IR_Type -> IR_Type -> Bool
type_match (TypeArray b1 _) (TypeArray b2 _) = type_match b1 b2
type_match t1 t2 = t1 `type_eq` t2 || t1 == TypeVoid || t2 == TypeVoid || t1 == NoType || t2 == NoType || is_generic t1 || is_generic t2
-- @TODO TypeVoid represents that the type couldn't be evaluated, for now!

function_rtype_match :: IR_Type -> IR_Type -> Bool
function_rtype_match rtype t = rtype `type_eq` t || t == TypeVoid || t == NoType


-- fala se o tipo não está resolvido (se ele for genérico).
unsolved_type :: IR_Type -> SemanticalContext Bool
unsolved_type (TypeGeneric g) = do
    -- @TODO buscar o tipo pra ver se tá resolvido de fato...
    -- sem inferência, qualquer genérico não tá resolvido.
    return $ True

unsolved_type t
    | have_generic t    = return $ True
    | otherwise         = return $ False


lift_lambda :: IR_Expression -> SemanticalContext Identifier
lift_lambda lambda@(ExpLambda rtype vars captures cmds) = do
    fc <- sc_get_fc

    let lambda_id = fc_lambda_count fc
    let sname = "@LAMBDA-" ++ show lambda_id

    -- vira mais um.
    sc_set_fc $ FC (fc_statement fc) (fc_has_return fc) (lambda_id + 1) (fc_context_level fc)

    st <- sc_get_st
    pos <- sc_get_pos
    vm <- sc_get_vm

    let captured = Map.restrictKeys vm $ Set.fromList captures
    let extended_vars = vars ++ [varstat_to_vardecl v | c <- captures, Just v <- [Map.lookup c captured]]

    set_captures sname captures

    let statement = FuncDef {
        symbol_name = sname,
        function_rtype = rtype,
        function_parameters = extended_vars,
        function_gtypes = [],
        function_body = cmds,
        symbol_pos = pos
    }

    sc_set_st $ st_insert statement st
    statement' <- verify_function statement captured
    sc_set_st $ st_insert statement' st

    --sc_raise $ ">>> LIFTED " ++ show sname 
    return $ sname


--------------------------
-- Reducing expressions --
--------------------------
-- @TODO may that move to IR.hs?

-- Rxpressions that can directly be represented in a simpler way may be.
-- Reduces basing on the literals, recursively. lots of pattern-matching...
reduce_expression :: IR_Expression -> IR_Expression

-- negation.
reduce_expression (ExpNegative (ExpLitInteger x))                   = ExpLitInteger $ - x
reduce_expression (ExpNegative (ExpLitFloating x))                  = ExpLitFloating $ - x
reduce_expression (ExpNegative (ExpLitBoolean b))                   = ExpLitBoolean $ not b

-- integer arithmethic. 
reduce_expression (ExpSum (ExpLitInteger x) (ExpLitInteger y))      = ExpLitInteger $ x + y
reduce_expression (ExpSub (ExpLitInteger x) (ExpLitInteger y))      = ExpLitInteger $ x - y
reduce_expression (ExpMul (ExpLitInteger x) (ExpLitInteger y))      = ExpLitInteger $ x * y
reduce_expression (ExpDiv (ExpLitInteger x) (ExpLitInteger y))      = ExpLitInteger $ div x y
reduce_expression (ExpIntDiv (ExpLitInteger x) (ExpLitInteger y))   = ExpLitInteger $ div x y
reduce_expression (ExpPow (ExpLitInteger x) (ExpLitInteger y))      = ExpLitInteger $ x ^ y

-- string arithmetic.
reduce_expression (ExpSum (ExpLitString x) (ExpLitString y))        = ExpLitString $ x ++ y
-- seems cool.
--reduce_expression (ExpSum (ExpLitString x) (ExpLitInteger y))       = ExpLitString $ replicate (fromIntegral y) x

-- @TODO float

-- logical.
reduce_expression (ExpAnd (ExpLitBoolean x) (ExpLitBoolean y))      = ExpLitBoolean $ x && y
reduce_expression (ExpAnd (ExpLitBoolean False) _)                  = ExpLitBoolean $ False
reduce_expression (ExpAnd _ (ExpLitBoolean False))                  = ExpLitBoolean $ False
reduce_expression (ExpOr (ExpLitBoolean x) (ExpLitBoolean y))       = ExpLitBoolean $ x || y
reduce_expression (ExpOr _ (ExpLitBoolean True))                    = ExpLitBoolean $ True
reduce_expression (ExpOr (ExpLitBoolean True) _)                    = ExpLitBoolean $ True

-- relational. @TODO float.
reduce_expression (ExpEq (ExpLitInteger x) (ExpLitInteger y))       = ExpLitBoolean $ x == y
reduce_expression (ExpNeq (ExpLitInteger x) (ExpLitInteger y))      = ExpLitBoolean $ x /= y
reduce_expression (ExpGt (ExpLitInteger x) (ExpLitInteger y))       = ExpLitBoolean $ x > y
reduce_expression (ExpGeq (ExpLitInteger x) (ExpLitInteger y))      = ExpLitBoolean $ x >= y
reduce_expression (ExpLt (ExpLitInteger x) (ExpLitInteger y))       = ExpLitBoolean $ x < y
reduce_expression (ExpLeq (ExpLitInteger x) (ExpLitInteger y))      = ExpLitBoolean $ x <= y

-- recursive.
reduce_expression (ExpSum e1 e2)                                    = reduce_bop ExpSum e1 e2
reduce_expression (ExpSub e1 e2)                                    = reduce_bop ExpSub e1 e2
reduce_expression (ExpMul e1 e2)                                    = reduce_bop ExpMul e1 e2
reduce_expression (ExpDiv e1 e2)                                    = reduce_bop ExpDiv e1 e2
reduce_expression (ExpIntDiv e1 e2)                                 = reduce_bop ExpIntDiv e1 e2
reduce_expression (ExpMod e1 e2)                                    = reduce_bop ExpMod e1 e2
reduce_expression (ExpPow e1 e2)                                    = reduce_bop ExpPow e1 e2

reduce_expression (ExpAnd e1 e2)                                    = reduce_bop ExpAnd e1 e2
reduce_expression (ExpOr e1 e2)                                     = reduce_bop ExpOr e1 e2
reduce_expression (ExpEq e1 e2)                                     = reduce_bop ExpEq e1 e2
reduce_expression (ExpNeq e1 e2)                                    = reduce_bop ExpNeq e1 e2
reduce_expression (ExpGt e1 e2)                                     = reduce_bop ExpGt e1 e2
reduce_expression (ExpGeq e1 e2)                                    = reduce_bop ExpGeq e1 e2
reduce_expression (ExpLt e1 e2)                                     = reduce_bop ExpLt e1 e2
reduce_expression (ExpLeq e1 e2)                                    = reduce_bop ExpLeq e1 e2

reduce_expression (ExpLIncr exp)                                    = ExpLIncr $ reduce_expression exp
reduce_expression (ExpRIncr exp)                                    = ExpRIncr $ reduce_expression exp
reduce_expression (ExpLDecr exp)                                    = ExpLDecr $ reduce_expression exp
reduce_expression (ExpRDecr exp)                                    = ExpRDecr $ reduce_expression exp

reduce_expression (ExpFCall sname exps)                             = ExpFCall sname            $ reduce_expression <$> exps
reduce_expression (ExpStructInstance sname exps)                    = ExpStructInstance sname   $ reduce_expression <$> exps
reduce_expression (ExpArrayInstancing exps)                         = ExpArrayInstancing        $ reduce_expression <$> exps

reduce_expression (ExpFCall_Implicit exp args)                      = ExpFCall_Implicit (reduce_expression exp) $ reduce_expression <$> args

-- @TODO reduce expression of lambda expression require an equivalent, mutually recursive, reduce_command...

-- everything else won't be reduced.
reduce_expression exp = exp

-- `reduce_expression` but tells if they're different.
reduce_expression' :: IR_Expression -> (Bool, IR_Expression)
reduce_expression' e =  let r = reduce_expression e
                        in  (r /= e, r)

-- attempts to reduce a binary operation, while it is possible.
reduce_bop constructor e1 e2 = 
    let (c1, r1) = reduce_expression' e1
        (c2, r2) = reduce_expression' e2
        result = constructor r1 r2
    in  if ((is_literal r1 || c1) && (is_literal r2 || c2)) then reduce_expression result
        else result 



is_literal :: IR_Expression -> Bool
is_literal (ExpLitInteger _) = True
is_literal _ = False

