{-  -------------------------------------
    @file       src/Backend/WASM/WAT_Codegen.hs
    @details    Compiles the IR into a WAT IR that can nextly be converted into WAT via pretty...
-}

module Backend.WASM.WAT_Codegen where

import Frontend.Pretty
import Frontend.IR
import Frontend.Token
import Frontend.Semantics ( SymbolTable )
import Frontend.Error

import Data.Map ( Map )
import qualified Data.Map as Map


---------
-- API --
---------

sl_to_wat :: SymbolTable -> Either Error WASM_Code
sl_to_wat st = 
    case codegen_context_run (translate_program st) default_state of
        Left err -> Left err
        Right (code, _) -> Right code


translate_program :: SymbolTable -> CodegenContext WASM_Code
translate_program st = do 
    structs <- compile_structs st

    let structs_and_keys = (\s@(CompiledStruct sname _) -> (sname, s)) <$> structs
    cc_set_sm $ Map.fromList structs_and_keys
    
    let functions = filter (not . is_struct) (snd <$> Map.toList st)
    wasm_functions <- mapM translate_function functions
    
    return $ WASM_Code wasm_functions



------------
-- State ---
-- & traversal data-structures...
------------

default_state :: CodegenState
default_state = CodegenState {
        cs_st           = Map.empty,
        cs_sm           = Map.empty,
        cs_locals       = Map.empty,
        cs_next_local   = 0,
        cs_inst         = [],
        cs_label_depth  = 0,
        cs_src_pos      = SrcPos (-1, -1)
    }


data CompiledStruct = CompiledStruct Identifier [(Identifier, WASM_Type)]
type LocalsMap = Map Identifier WASM_Type
type StructMap = Map Identifier CompiledStruct


data CodegenState = 
    CodegenState {
        cs_st           :: SymbolTable,            
        cs_sm           :: StructMap,            
        cs_locals       :: LocalsMap,           -- defines the current local enviroment in the WASM context.
        cs_next_local   :: Int,                 -- counter (index where to allocate next local)
        cs_inst         :: [WASM_Instruction],  -- accumulated WASM instructions generated list
        cs_label_depth  :: Int,                 -- controls the block's depth to branch operations
        cs_src_pos      :: SrcPos
    }


newtype CodegenContext v = CC {
    codegen_context_run :: CodegenState -> Either Error (v, CodegenState)
}



instance Functor CodegenContext where
    -- applying a function to the context is equivalent 
    -- to applying it to the resultant value.
    fmap f (CC translator) = CC $ \state -> 
        let r = translator state
        in
        case r of 
            -- propagates the error,
            Left err                -> Left err

            -- applies the function to the resultant computation.
            Right (value, state')   ->
                Right (f value, state')


instance Applicative CodegenContext where
    pure x = CC $ \state ->
        -- just encapsulates the parameter.
        -- symbol table isn't used.
        Right (x, state)

    (CC cc_f) <*> (CC x) = CC $ \state -> let
        -- executing the function parameter inside context,
        result_f = cc_f state

        in
        case result_f of 
            -- propagates the error,
            Left err            -> Left err

            Right (f, state')   ->                -- executing the value interpreter,
                let result_x = x state'
                in
                case result_x of 
                    -- propagating the error,
                    Left err            -> Left err

                    -- or then applying the computed function to the computed value.
                    Right (v, state'')  ->
                        Right (f v, state'')


instance Monad CodegenContext where
    -- alias
    return = pure

    -- (>>=) :: m a -> (a -> m b) -> m b
    -- to sequence actions, 
    (CC interpreter) >>= k = 
        CC $ \state -> let
            -- firstly, interprets the first action,
            r = interpreter state
            in
            case r of 
                Left err                -> Left err
                Right (value, state')   -> let
                    -- then interprets the second.
                    CC interpreter' = k value
                    
                    r' = interpreter' state'
                    in
                    case r' of 
                        e2@(Left _)     -> e2
                        final@(Right _) -> final



-- getters and setters.
cc_get_st :: CodegenContext SymbolTable
cc_get_st = CC $ \state -> Right (cs_st state, state)

cc_get_sm :: CodegenContext StructMap
cc_get_sm = CC $ \state -> Right (cs_sm state, state)

cc_get_locals :: CodegenContext LocalsMap
cc_get_locals = CC $ \state -> Right (cs_locals state, state)

cc_get_next_local :: CodegenContext Int
cc_get_next_local = CC $ \state -> Right (cs_next_local state, state)

cc_get_inst :: CodegenContext [WASM_Instruction]
cc_get_inst = CC $ \state -> Right (cs_inst state, state)

cc_get_label_depth :: CodegenContext Int
cc_get_label_depth = CC $ \state -> Right (cs_label_depth state, state)


cc_set_st :: SymbolTable -> CodegenContext ()
cc_set_st st = CC $ \s -> Right ((), s { cs_st = st })

cc_set_sm :: StructMap -> CodegenContext ()
cc_set_sm sm = CC $ \s -> Right ((), s { cs_sm = sm })

cc_set_locals :: LocalsMap -> CodegenContext ()
cc_set_locals l = CC $ \s -> Right ((), s { cs_locals = l })

cc_set_next_local :: Int -> CodegenContext ()
cc_set_next_local nl = CC $ \s -> Right ((), s { cs_next_local = nl })

cc_set_inst :: [WASM_Instruction] -> CodegenContext ()
cc_set_inst inst = CC $ \s -> Right ((), s { cs_inst = inst } )

cc_set_label_depth :: Int -> CodegenContext ()
cc_set_label_depth ld = CC $ \s -> Right ((), s { cs_label_depth = ld })

cc_set_pos :: SrcPos -> CodegenContext ()
cc_set_pos pos = CC $ \s -> Right ((), s { cs_src_pos = pos })


raise :: String -> CodegenContext ()
raise error_msg_ = CC $ \state ->
    Left (Error CodegenError error_msg_ (cs_src_pos state))


--------------
-- WASM IR ---
--------------

-- OBS: SL identifiers are a subset of the WAT identifiers, s.t. we'll not going to get in
-- trouble with direct conversion...
to_wat_identifier :: Identifier -> Identifier
to_wat_identifier = id


-- The WASM code resepresentaion as a data structure.
data WASM_Code = WASM_Code [WASM_Function]


data WASM_Function = 
    WF { 
        wf_name     :: String, 
        wf_params   :: [WASM_Type], 
        wf_result   :: Maybe WASM_Type, 
        wf_body     :: [WASM_Instruction]
    }
    deriving (Eq, Show)


data WASM_Type = I32 | I64 | F32 | F64 
    deriving (Eq, Show)


-- PURE.
to_wasm_type :: IR_Type -> Maybe WASM_Type
to_wasm_type TypeInt   = Just I32
to_wasm_type TypeBool  = Just I32
to_wasm_type TypeFloat = Just F32
to_wasm_type TypeVoid  = Nothing     -- Nothing will mean no type


-- DIFFERS FROM WHAT IT IS INSIDE THE CONTEXT ONLY.
wasm_type :: IR_Type -> CodegenContext WASM_Type
wasm_type the_type = do
    case to_wasm_type the_type of
        Nothing -> do
            raise $ "Can't map type " ++ pretty_sl the_type ++ " to WASM"
            return $ F64

        Just t  -> return $ t



data WASM_Instruction = 
    -- I32 data type.
    I32_Const    Int |
    I32_Add | I32_Sub | I32_Mul | I32_DivS |  I32_DivU | I32_RemS |  I32_RemU | 
    I32_And | I32_Or | I32_Xor | 
    I32_Shl | I32_ShrS | I32_ShrU | I32_Rotl | I32_Rotr | 

    I32_Eq | I32_Ne | 
    I32_LtS | I32_GtS | I32_LeS | I32_GeS | 
    I32_LtU | I32_GtU | I32_LeU | I32_GeU | 

    I32_Load | I32_Store |

    -- @TODO operações de truncar

    -- Stack control.
    LocalDecl   Identifier WASM_Type |
    LocalGet    Identifier |
    LocalSet    Identifier |
    Drop |

    -- Function-related.
    Call        Identifier |
    WReturn | 

    -- Scope.
    Block       Identifier [WASM_Instruction] | 
    Loop        Identifier [WASM_Instruction] | 

    -- Control-flux.
    WIf       (Maybe WASM_Type) [WASM_Instruction] [WASM_Instruction] |
    Br          Identifier |
    BrIf        Identifier
    
    deriving (Eq, Show)



---------------------
-- Code Generation --
---------------------

compile_structs :: SymbolTable -> CodegenContext [CompiledStruct]
compile_structs st = do
    let struct_statements = filter is_struct (snd <$> Map.toList st)
    mapM compile_struct struct_statements  


is_struct :: IR_Statement -> Bool
is_struct (StructDef _ _ _) = True
is_struct _                 = False


translate_field :: IR_Var -> CodegenContext (Identifier, WASM_Type)
translate_field (VarDecl s vtype) = do
    vtype' <- wasm_type vtype
    return $ (s, vtype')
            

compile_struct :: IR_Statement -> CodegenContext CompiledStruct
compile_struct (StructDef sname fields pos) = do
    cc_set_pos pos
    
    fields' <- mapM translate_field fields
    return $ CompiledStruct sname fields'


translate_function :: IR_Statement -> CodegenContext WASM_Function
translate_function (FuncDef fname rtype params gtypes body pos) = do
    cc_set_pos pos

    -- convertendo os tipos.
    let param_types = (\(VarDecl _ t) -> t) <$> params
    wparam_types <- mapM wasm_type param_types

    let wrtype = to_wasm_type rtype
    
    instructions <- mapM translate_command body
    return $ WF fname wparam_types wrtype (concat instructions)


translate_command :: IR_LocatedCommand -> CodegenContext [WASM_Instruction]
translate_command _ = return $ []
{-
translate_command LC ((VarDef var exp), pos) = []

translate_command LC ((Assignment varaccess exp), pos) = []

translate_command LC ((Return exp), pos) = []

translate_command LC ((If exp cmds1 cmds2), pos) = []

translate_command LC ((While exp cmds), pos) = []

translate_command LC ((For init_cmd exp it_exp cmds), pos) = []

translate_command LC ((CmdExpression exp), pos) = []

translate_command LC ((Print exp), pos) = []

translate_command LC ((Scan exp), pos) = []

translate_command LC ((CmdList cmds), pos) = []

-}


