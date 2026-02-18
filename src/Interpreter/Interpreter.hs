{-  ------------------------------------------
    @file       src/Interpreter/Interpreter.hs
    @brief      INTERPRETA A REPRESENTAÇÃO INTERMEDIÁRIA.
    @TODO       mostrar posição exata do erro de execução...
-}

module Interpreter.Interpreter (    interpret, 
                                    interpret_expression, 
                                    get_rc_and_log, 
                                    InterpreterResult,
                                    ProgramLog, 
                                    InterpreterState(..),
                                    Value(..)) where

import Frontend.Value

import Frontend.Error ( Error(Error), ErrorType(ExecutionError) )
import Frontend.Token
import Frontend.IR
import Frontend.Semantics
import Frontend.Parser ( parse_sl )

import Data.Map
import Data.List (elemIndex)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Map as Map

-- import Control.Monad (zipWithM)

-- @todo use the except monad, perhaps maybe...
--import Control.Monad.Trans.Except



---------
-- API --
---------

interpret_string :: String -> IO InterpreterResult
interpret_string src_code = do
    case parse_sl src_code of 
        Left err        -> return $ Left err
        Right program   -> interpret program


interpret :: IR_Program -> IO InterpreterResult
interpret p__ = do
    case sl_verify p__ of
        Left err -> do
            --print err
            return $ Left err

        Right (_, st) -> do
            start_time <- getCurrentTime
            
            result <- interpreter_context_run ic_interpret_program (base_is st)

            end_time <- getCurrentTime

            putStrLn $ "End. Elapsed: " ++ show (diffUTCTime end_time start_time) ++ "."
            
            case result of
                Left err -> print err -- TODO
                Right (v, _) -> do
                    putStrLn $ "Result: " ++ show v
                    return ()

            return result



-- @TODO pra facilitar os testes
interpret_expression :: IR_Expression -> IO ()
interpret_expression _exp =
    print "vazio"


get_rc_and_log :: InterpreterResult -> (Value, ProgramLog)
get_rc_and_log (Left _) = (ValueUnknown, [])
get_rc_and_log (Right (v, state)) = (v, is_log state)



----------------
-- Base types --
----------------

--
type InterpreterResult = Either Error (Value, InterpreterState)


-- Variable-value table representing memory.
type ProgramMemory = Map Identifier Value

-- Tracks what is printed by the program.
type ProgramLog = [String]

--
data InterpreterState = InterpreterState {
    is_st       :: SymbolTable,
    is_pm       :: ProgramMemory,
    is_gm       :: GenericsMap,
    is_log      :: ProgramLog,
    is_src_pos  :: SrcPos
} deriving (Eq, Show)


-- Program's execution context.
-- Holds the symbol-table, the program's memory, and
-- propagates error and values.
newtype InterpreterContext v = IC {
    --interpreter_context_run :: SymbolTable -> ProgramMemory -> IO (Either String (v, ProgramMemory))
    interpreter_context_run :: InterpreterState -> IO (Either Error (v, InterpreterState))
}


base_is :: SymbolTable -> InterpreterState
base_is st = InterpreterState st pm gm _log _pos where
    pm = base_pm
    _log = []
    _pos = SrcPos (- 1, - 1)
    gm = Map.empty


base_pm :: ProgramMemory
base_pm = Map.insert "@rc" ValueUnknown Map.empty 


ic_get_st :: InterpreterContext SymbolTable
ic_get_st = IC $ \state ->
    return $ Right (is_st state, state)


ic_get_pm :: InterpreterContext ProgramMemory
ic_get_pm = IC $ \state ->
    return $ Right (is_pm state, state)


ic_set_pm :: ProgramMemory -> InterpreterContext ()
ic_set_pm pm = IC $ \(InterpreterState st _ gm _log _pos) ->
    return $ Right ((), InterpreterState st pm gm _log _pos)


ic_get_gm :: InterpreterContext GenericsMap
ic_get_gm = IC $ \state -> 
    return $ Right (is_gm state, state)

ic_set_gm :: GenericsMap -> InterpreterContext ()
ic_set_gm gm = IC $ \(InterpreterState st pm _ _log _pos) ->
    return $ Right ((), InterpreterState st pm gm _log _pos)

ic_raise :: String -> InterpreterContext v
ic_raise error_msg_ = IC $ \state -> do
    --_ <- return $ Right $ ic_print $ error_msg_
    return $ Left (Error ExecutionError error_msg_ (is_src_pos state))


-- scapes the IC context to perform IO...
ic_io :: IO v -> InterpreterContext v
ic_io io = IC $ \state -> do
    x <- io
    return $ Right (x, state)


ic_if :: Bool -> InterpreterContext v -> InterpreterContext v -> InterpreterContext v
ic_if True action1 _    = action1
ic_if False _ action2   = action2


-- perform the IO and register the string on the program log.
ic_log :: String -> (String -> IO v) -> InterpreterContext v
ic_log string io = IC $ \(InterpreterState st pm gm _log _pos) -> do
    x <- io string

    -- constrói novo estado.
    return $ Right (x, InterpreterState st pm gm (_log ++ [string]) _pos)



---------------
-- Instances --
---------------

instance Functor InterpreterContext where
    -- applying a function to the context is equivalent 
    -- to applying it to the resultant value.
    fmap f (IC interpreter) = IC $ \state -> do
        -- executing the base-interpreter,
        r <- interpreter state

        case r of 
            -- propagates the error,
            Left err                -> return $ Left err

            -- applies the function to the resultant computation.
            Right (value, state')   ->
                return $ Right (f value, state')


instance Applicative InterpreterContext where
    pure x = IC $ \state -> do
        -- just encapsulates the parameter.
        -- symbol table isn't used.
        return $ Right (x, state)

    (IC ic_f) <*> (IC x) = IC $ \state -> do
        -- executing the function parameter inside context,
        result_f <- ic_f state

        case result_f of 
            -- propagates the error,
            Left err            -> return $ Left err

            Right (f, state')   -> do
                -- executing the value interpreter,
                result_x <- x state'

                case result_x of 
                    -- propagating the error,
                    Left err            -> return $ Left err

                    -- or then applying the computed function to the computed value.
                    Right (v, state'')  -> do
                        return $ Right (f v, state'')


instance Monad InterpreterContext where
    -- alias
    return = pure

    -- (>>=) :: m a -> (a -> m b) -> m b
    -- to sequence actions, 
    (IC interpreter) >>= k = 
        IC $ \state -> do
            -- firstly, interprets the first action,
            r <- interpreter state
            case r of 
                Left err                -> return $ Left err
                Right (value, state')   -> do
                    -- then interprets the second.
                    let IC interpreter' = k value
                    
                    r' <- interpreter' state'
                    case r' of 
                        e2@(Left _)     -> return $ e2
                        final@(Right _) -> return $ final



-------------
-- Program --
-------------

-- Executes a IR_Program; returns the main's RC.
ic_interpret_program :: InterpreterContext Value
ic_interpret_program = do
    st <- ic_get_st

    -- extracting the main function,
    let     maybe_main = st_get_main st
    case    maybe_main of
        Nothing     -> ic_raise $ "Invalid `main`." -- @TODO more informative error.
        Just main@(FuncDef _ _ _ _ _ _) -> do
            let n_parameters = length $ function_parameters main :: Int

            -- deciding how to run it depending on the # of parameters...
            (rc, _) <- if n_parameters == 0 then do
                ic_interpret_function main []

            else if (n_parameters == 2) then
                ic_interpret_function main [ValueInt 2, ValueArray [ValueString "programa", ValueString "qualquer coisa"]]

            else
                ic_raise $ "ERRO DE ARGUMENTOS DA MAIN"

            return $ rc
        
        _ -> ic_raise $ "`main` defined as struct..."


-- Attempts getting the function `main` on the ST.
st_get_main :: SymbolTable -> Maybe IR_Statement
st_get_main st = 
    case Map.lookup "main" st of 
        Nothing                             -> Nothing
        Just (StructDef _ _ _)              -> Nothing
        Just f@(FuncDef _ _ _ _ _ _)        -> Just f



---------------------------
-- Interpreting function --
---------------------------

-- takes in the IR function description and a list of values (its args).
ic_interpret_function :: IR_Statement -> [Value] -> InterpreterContext (Value, ProgramMemory)
ic_interpret_function (FuncDef fname rtype param gtypes body _) args = do
    --ic_io $ putStrLn $ "=== Executando função:"
    --ic_io $ putStrLn $ pretty_sl f

    -- saving older program state...    
    gm <- ic_get_gm
    pm <- ic_get_pm
    
    ic_set_pm $ base_pm

    -- loading the function context (arguments).
    ic_function_load_args args param fname

    -- running command list (function's return will be saved there.)
    (does_return, value) <- ic_interpret_command_list body
    --ic_io $ putStrLn $ "=== Terminou de executar a função "

    final_value <- case (does_return, rtype) of
        (True, _)           -> return value
        (False, TypeVoid)   -> return ValueUnknown
        (False, _)          -> ic_raise $ "Function `" ++ fname ++ "` ended without return."

    pm' <- ic_get_pm
    
    -- resetting earlier state (backup implicitly based on stack)...
    ic_set_pm pm
    ic_set_gm gm
    
    return $ (final_value, pm')

ic_interpret_function _ _ = do
    ic_raise $ "CHAMANDO ERRADO INTERPRETA FUNÇÃO" -- This should never happen


-- checks the function's parameters and arguments and load the information into memory.
-- function name parameter is just for erroeing.
ic_function_load_args :: [Value] -> [IR_Var] -> Identifier -> InterpreterContext ()
ic_function_load_args vs ps fname = do
    -- # args gotta be the same.
    if length vs == length ps then return $ ()
    else ic_raise $ "Wrong # of arguments (" ++ (show $ length vs) ++ " of " ++ (show $ length ps) ++ ") to function `" ++ fname ++ "`"

    ic_function_load_args_rec vs ps

-- same motivation as above, but this one is recursive.
ic_function_load_args_rec :: [Value] -> [IR_Var] -> InterpreterContext ()
ic_function_load_args_rec [] [] = return $ ()
ic_function_load_args_rec (v:vs) ((VarDecl vname vtype):ps)
    -- generic overloading...
    | have_generic vtype = do
        solve_generics v vtype

        pm <- ic_get_pm
        ic_set_pm $ Map.insert vname v pm

        ic_function_load_args_rec vs ps

    -- compatible types OR type inference...
    | é_tipo_válido v vtype || vtype == TypeVoid = do
        pm <- ic_get_pm
        ic_set_pm $ Map.insert vname v pm

        -- tail rec. call.
        ic_function_load_args_rec vs ps

    -- just erroeing...
    | otherwise = ic_raise $ "Invalid argument type: " ++ show v ++ " / " ++ show vtype ++ show (length vs)

ic_function_load_args_rec _ _ = ic_raise "Lenght mismatch in function load argumets" -- should never happen...
    

-- Solves the generic types from the given value.
solve_generics :: Value -> IR_Type -> InterpreterContext ()
solve_generics v t@(TypeGeneric g) = do
    --gm <- ic_get_gm
    overload_generic (to_type v) t


-- assuming the arrays are always homogeneous, we look only to the first element.
solve_generics (ValueArray (v:_)) (TypeArray t _) = solve_generics v t

-- oh god...
solve_generics (ValueFunction fname) (TypeFunction ts tr) = do
    st <- ic_get_st

    case Map.lookup fname st of
        Just (FuncDef _ rtype param [] _ _)    -> do
            overload_generic rtype tr
            overload_generic_list ts ((\(VarDecl _ t) -> t) <$> param)

        _                           -> ic_raise $ "Function " ++ fname ++ "not defined."

solve_generics _ _ = ic_raise "....."

overload_generic_list :: [IR_Type] -> [IR_Type] -> InterpreterContext ()
overload_generic_list [] [] = return $ ()
overload_generic_list (a:as) (b:bs) = do 
    overload_generic a b
    overload_generic_list as bs
overload_generic_list _ _ = ic_raise "Size mismatch for generics overload" -- should never happen


overload_generic :: IR_Type -> IR_Type -> InterpreterContext ()
overload_generic vtype (TypeGeneric g) = do
    gm <- ic_get_gm
    case Map.lookup g gm of
        -- genérico ainda não tá sobrecarregado.
        Nothing ->  do
            ic_set_gm $ Map.insert g vtype gm
            return $ ()
        Just t  -> 
            case tipo_compatível vtype t of
                True    -> return $ ()
                False   -> ic_raise $ "Couldn't overload generic `" ++ g ++ "` (determined " ++ show t ++ ")" 
overload_generic _ _ = return $ ()


-- só pra ver se o Value concorda com o IR_Type...
é_tipo_válido :: Value -> IR_Type -> Bool
é_tipo_válido (ValueInt _) TypeInt                  = True
é_tipo_válido (ValueFloat _) TypeFloat              = True
é_tipo_válido (ValueString _) TypeString            = True
é_tipo_válido (ValueBool _) TypeBool                = True
é_tipo_válido (ValueArray b) (TypeArray t _)        = é_tipo_válido (b !! 0) t
é_tipo_válido (ValueFunction _) (TypeFunction _ _)  = True
é_tipo_válido (ValueStruct sname1 _) (TypeStruct sname2) = sname1 == sname2
é_tipo_válido _ _                                   = False

tipo_compatível :: IR_Type -> IR_Type -> Bool
tipo_compatível a b = a == b



-- CONVERTE PRO VALOR NO CONTEXTO.
-- PRECISA DISSO PQ TEM QUE AVALIAR A EXPRESSÃO E TUDO MAIS.
to_value :: IR_Type -> InterpreterContext Value
to_value TypeVoid           = return $ ValueUnknown
to_value TypeBool           = return $ ValueBool False
to_value TypeInt            = return $ ValueInt 0
to_value TypeFloat          = return $ ValueFloat 0.0
to_value TypeString         = return $ ValueString "----EMPTY----"
to_value (TypeArray base [_exp]) = do
    (value, _) <- ic_interpret_expression _exp
    case value of
        ValueInt n  ->
            if n <= 0 then do
                ic_raise $ "Invalid index: negative (" ++ show n ++ ")"

            else do
                bolsa_de_valores <- mapM to_value (replicate (fromIntegral n) base)
                return $ ValueArray $ bolsa_de_valores

        _          -> ic_raise $ "Invalid index type: " ++ show value

to_value (TypeArray _ _)    = error $ "Multi-dimensional arrays: NYI"
to_value (TypeGeneric g) = do
    gm <- ic_get_gm
    case Map.lookup g gm of
        Just t  -> to_value t
        _       -> ic_raise $ "Generic type " ++ show g ++ " is unknown"

to_value (TypeStruct sname)    = do
    -- verificando se tá na tabela de símbolos (para estruturas)...
    st <- ic_get_st

    case Map.lookup sname st of
        Just (StructDef _sname _fields _)   -> do
            value_list <- mapM (\(VarDecl _ field_type) -> to_value field_type) _fields
            return $ ValueStruct _sname $ value_list
        _                               -> do
            ic_raise $ "Struct " ++ show sname ++ " is unknown"

to_value (TypeFunction _ _) = error $ "TYPE FUNCTION"
to_value (NoType) = error $ "NoType on interpreter" -- should never happen


---------------------------
-- Interpreting commands --
---------------------------

-- important to save function's return value!!
ic_interpret_command_list :: [IR_LocatedCommand] -> InterpreterContext (Bool, Value)
ic_interpret_command_list [] = return $ (False, ValueUnknown)
ic_interpret_command_list (cmd:cmds) = do
    --ic_io $ putStrLn $ "=== Vô executar " ++ pretty_sl cmd
    (does_return, v) <- ic_interpret_command cmd

    case does_return of 
        False   -> ic_interpret_command_list cmds
        
        -- in case the command was a return, then 'halts' the list execution and
        -- propagates the value.
        True    -> do
            return $ (True, v)


-- Interprets a command, and returns whether the result is returned, 
-- and the associated computed value.
ic_interpret_command :: IR_LocatedCommand -> InterpreterContext (Bool, Value)
ic_interpret_command (LC (VarDef (VarDecl vname vtype) vexp) _) = do
    --ic_io $ putStrLn $ "=== Definindo variável `" ++ vname ++ "` (" ++ show vtype ++ ")"
    
    (value, _) <- ic_interpret_expression vexp
    case é_tipo_válido value vtype of
        False   -> do
            -- x : int;
            -- ValueUnknown passa.

            if value == ValueUnknown then do                
                value' <- to_value vtype 
                pm <- ic_get_pm
                ic_set_pm (Map.insert vname value' pm)
                return $ (False, value')

            else if vtype == TypeVoid then do 
                pm <- ic_get_pm
                ic_set_pm (Map.insert vname value pm)
                return $ (False, value)
            
            else ic_raise $ "Type incompatibility: " ++ show value ++ " with " ++ show vtype ++ " (variable: " ++ show vname ++ ")"

        True    -> do
            pm <- ic_get_pm
            ic_set_pm (Map.insert vname value pm)

            return $ (False, value)


ic_interpret_command (LC (Return _exp) _) = do

    (lvalue, _) <- ic_interpret_expression _exp

    --ic_io $ putStrLn $ "=== Retorno: " ++ pretty_sl exp ++ "--->" ++ show lvalue
    return $ (True, lvalue)


ic_interpret_command (LC (Assignment varaccess _exp) _) = do
    -- for instance, for assignment, the reference doesn't matter.
    -- it may be important if pointers turn out to be a language's feature thought.
    (lvalue, _) <- ic_interpret_expression _exp
    
    --ic_io $ putStrLn $ "=== Atribuindo " ++ pretty_sl exp ++ "(" ++ show lvalue ++ ") para " ++ pretty_sl varaccess
    non_return_command $ ic_pm_write varaccess lvalue


ic_interpret_command (LC (If _exp cmds1 cmds2) _) = do
    -- rvalue is irrelavant.
    (lvalue, _) <- ic_interpret_expression _exp

    case lvalue of
        ValueBool b -> do
            -- simply: chooses between the list to be executed...
            ic_if b (ic_interpret_command_list cmds1) (ic_interpret_command_list cmds2)
        _ -> ic_raise $ "Non-boolean type on if command: " ++ show lvalue


ic_interpret_command w@(LC (While _exp cmds) _) = do

    (lvalue, _) <- ic_interpret_expression _exp
    --ic_io $ putStrLn $ "=== Cabeça do While: " ++ show lvalue

    case lvalue of 
        ValueBool b -> do
            -- depending on the boolean-value, executes
            -- the commands and then repeats the while command.
            ic_if b (ic_interpret_command_list cmds >> ic_interpret_command w) (return (False, ValueUnknown))

        _ -> ic_raise $ "Non-boolean type on While command: " ++ show lvalue

ic_interpret_command for@(LC (For init_cmd _exp it_exp cmds) _) = do
    
    -- saving state.
    pm <- ic_get_pm

    (_, _) <- case lc_cmd init_cmd of 
        Assignment (VarAccess vname VarAccessNothing) _exp   -> ic_interpret_command (LC (VarDef (VarDecl vname TypeVoid) _exp) $ lc_pos init_cmd)
        _                                                   -> ic_interpret_command init_cmd

    -- once the initial command is executed, that is essentially a while with an extra command at the end.
    (_, v) <- ic_interpret_command $ LC (While _exp (cmds ++ [LC (CmdExpression it_exp) $ lc_pos init_cmd])) $ lc_pos init_cmd

    -- erasing variables created inside the block.
    -- specially important for 'for', since usually init_cmd goes with variable def.
    pm' <- ic_get_pm
    ic_set_pm $ Map.restrictKeys pm' (Map.keysSet pm)

    return $ (False, v)

        
ic_interpret_command (LC (CmdExpression _exp) _) = do
    (lvalue, _) <- ic_interpret_expression _exp
    non_return_command $ (pure lvalue)
        

ic_interpret_command (LC (Print _exp) _) = do
    (lvalue, rvalue) <- ic_interpret_expression _exp
    
    case lvalue of
        ValueInt x      -> ic_print $ "INT: " ++ show x
        ValueFloat x    -> ic_print $ "FLOAT: " ++ show x
        ValueBool x     -> ic_print $ "BOOL: " ++ show x
        ValueString x   -> ic_print $ "STRING: " ++ show x
        ValueUnknown    -> undefined
        _               -> undefined

    ic_pm_write (VarAccess "@rc" VarAccessNothing) (ValueInt 0)
    non_return_command $ (pure lvalue)

ic_interpret_command (LC (Scan _exp) _) = ic_raise "scan NYI"

ic_print :: String -> InterpreterContext ()
ic_print string = 
    ic_log string putStrLn


non_return_command = fmap (\x -> (False, x))




------------------------------
-- Interpreting expressions --
------------------------------

-- Reads a variable's lvalue in memory.
ic_pm_read :: IR_VarAccess -> InterpreterContext Value

-- Accessing a variable in the ST.
ic_pm_read (VarAccess vname next_access) = do
    pm <- ic_get_pm
    case Map.lookup vname pm of 
        Nothing -> ic_raise $ "Undef access to " ++ show vname 
        Just x  -> do 
            x' <- ic_pm_read2 next_access x
            --ic_io $ putStrLn $ vname ++ " = " ++ show x'
            return $ x'

ic_pm_read VarAccessNothing = ic_raise "ic_pm_read on VarAccessNothing" -- should never happen
ic_pm_read (VarAccessIndex _ _) = ic_raise "ic_pm_read on VarAccessIndex" -- should never happen

ic_pm_read2 :: IR_VarAccess -> Value -> InterpreterContext Value
ic_pm_read2 VarAccessNothing acc = return $ acc
ic_pm_read2 (VarAccessIndex index_exp next_access) acc = do
    case acc of 
        ValueArray xs           -> do
            (index, _) <- ic_interpret_expression index_exp
            case index of
                ValueInt n      -> do
                    ic_pm_read2 next_access (xs !! (fromIntegral n))
                _               -> ic_raise $ "NÃO PODE PQ ÍNDICE N É INT"
        _                       -> ic_raise $ "NÃO PODE INDEXAR COISA QUE NÃO É ARRAY UAI"
        
ic_pm_read2 (VarAccess vname next_access) acc = do
    case acc of 
        ValueStruct sname xs                -> do

            st <- ic_get_st
            case Map.lookup sname st of 
                Just (StructDef _ fields _) -> do
                    case elemIndex vname $ (\(VarDecl name _) -> name) <$> fields of
                        Just i              -> do
                            ic_pm_read2 next_access (xs !! i)
                        _                   -> ic_raise $ "NÃO ACHOU O CAMPO"
                _                           -> ic_raise $ "NUNCA DEVERIA ACONTECER" 
            
        ValueArray xs                       -> do
            case next_access of
                VarAccessNothing    -> do
                    if vname == "size" then do
                        return $ ValueInt $ toInteger $ length xs 
                    else ic_raise $ "ARRAY NÃO TEM ESSE CAMPO"
                _                   -> ic_raise $ "ACESSO INVÁLIDO EM ARRAY..."

        _                           -> ic_raise $ "UÉ CARALHO O TREM NEM ESTRUTURA É"

-- splitAt :: Int -> [a] -> ([a], [a])
update_at :: Int -> a -> [a] -> [a]
update_at i x xs =
  case Prelude.splitAt i xs of
    (pre, _ : post) -> pre ++ x : post
    _               -> xs   -- índice fora do range

-- Writes a variable's lvalue in memory.
ic_pm_write :: IR_VarAccess -> Value -> InterpreterContext Value
ic_pm_write (VarAccess vname next_access) value = do
    pm <- ic_get_pm
    case Map.lookup vname pm of
        Nothing -> do
            ic_print $ "UUUUUUAUUUUUUUUU"
            ic_raise $ "Undef access to " ++ show vname
        Just x  -> do
            --ic_io $ putStrLn $ "SETTING " ++ show vname ++ "(" ++ show x ++ ") = " ++ show value
            
            x' <- ic_pm_write2 next_access x value
            ic_set_pm (Map.insert vname x' pm)
            return $ x


ic_pm_write2 :: IR_VarAccess -> Value -> Value -> InterpreterContext Value
ic_pm_write2 VarAccessNothing acc valor = return $ valor -- @TODO VERIFICAR SE ACC BATE COM VALOR NESSE CASO BASE.
ic_pm_write2 (VarAccessIndex index_exp next_access) acc valor = do
    case acc of
        ValueArray xs           -> do
            (index, _) <- ic_interpret_expression index_exp
            case index of
                ValueInt n      -> do
                    x <- ic_pm_write2 next_access (xs !! (fromIntegral n)) valor
                    let xs' = update_at (fromIntegral n) x xs 
                    return $ ValueArray xs'
                _               -> ic_raise $ "NÃO PODE PQ ÍNDICE N É INT2"
        _                       -> do
            pm <- ic_get_pm
            ic_raise $ "NÃO PODE INDEXAR COISA QUE NÃO É ARRAY UAI2. Exp = " ++ show index_exp ++ "; acc = " ++ show acc

ic_pm_write2 (VarAccess vname next_access) acc valor = do
    case acc of 
        ValueStruct sname xs                -> do
            st <- ic_get_st
            case Map.lookup sname st of 
                Just (StructDef _ fields _)   -> do
                    case elemIndex vname $ (\(VarDecl name _) -> name) <$> fields of
                        Just i              -> do
                            x <- ic_pm_write2 next_access (xs !! i) valor
                            let xs' = update_at (fromIntegral i) x xs
                            return $ ValueStruct sname xs' 
                        _                   -> ic_raise $ "NÃO ACHOU O CAMPO2"
                _                           -> ic_raise $ "NUNCA DEVERIA ACONTECER2" 
        _                                   -> ic_raise $ "UÉ CARALHO O TREM NEM ESTRUTURA É2"


ic_interpret_expression :: IR_Expression -> InterpreterContext (Value, Reference)
ic_interpret_expression ExpNothing              = return $ (ValueUnknown, ReferênciaNão)
ic_interpret_expression (ExpVariable varaccess@(VarAccess vname _)) = do
    
    -- actually access to variable can be access to symbols as well...
    -- as an effect, we first try to access it normally as a variable to then attempt
    -- interpreting it as a symbol "reference".
    pm <- ic_get_pm
    lvalue <- case Map.lookup vname pm of
        -- didn't find -> go try the symbol.
        Nothing -> do
            st <- ic_get_st
            case Map.lookup vname st of
                Just (FuncDef _ _ _ _ _ _)  -> return $ ValueFunction vname
                _                           -> ic_raise $ "NEM VARIÁVEL NEM SÍMBOLO KK"
        _                                   -> ic_pm_read varaccess

    --pm <- ic_get_pm
    --ic_io $ putStrLn $ "=== ExpVariable: " ++ show lvalue ++ ", PM: " ++ show pm

    return $ (lvalue, Referência varaccess)

ic_interpret_expression (ExpVariable (VarAccessIndex _ _)) = ic_raise "Invalid index access"

ic_interpret_expression (ExpVariable VarAccessNothing) = return $ (ValueUnknown, ReferênciaNão) -- should never happen
    
ic_interpret_expression (ExpLitInteger x)       = return $ (ValueInt x, ReferênciaNão)
ic_interpret_expression (ExpLitFloating x)      = return $ (ValueFloat x, ReferênciaNão)
ic_interpret_expression (ExpLitBoolean x)       = return $ (ValueBool x, ReferênciaNão)
ic_interpret_expression (ExpLitString x)        = return $ (ValueString x, ReferênciaNão)

ic_interpret_expression (ExpSum e1 e2)          = ic_interpret_binary_operation e1 e2 (||+||)
ic_interpret_expression (ExpSub e1 e2)          = ic_interpret_binary_operation e1 e2 (||-||)
ic_interpret_expression (ExpMul e1 e2)          = ic_interpret_binary_operation e1 e2 (||*||)
ic_interpret_expression (ExpDiv e1 e2)          = ic_interpret_binary_operation e1 e2 (||/||)
ic_interpret_expression (ExpIntDiv e1 e2)       = ic_interpret_binary_operation e1 e2 (||//||)
ic_interpret_expression (ExpMod e1 e2)          = ic_interpret_binary_operation e1 e2 (||%||)
ic_interpret_expression (ExpPow e1 e2)          = ic_interpret_binary_operation e1 e2 (||**||)

ic_interpret_expression (ExpNegative e) = do
    (v, r) <- ic_interpret_expression e
    return $ ((||!||) v, r)

ic_interpret_expression (ExpAnd e1 e2)          = ic_interpret_binary_operation e1 e2 (||&&||)
ic_interpret_expression (ExpOr e1 e2)           = ic_interpret_binary_operation e1 e2 (||||||)

ic_interpret_expression (ExpEq e1 e2)           = ic_interpret_binary_operation e1 e2 (||==||)
ic_interpret_expression (ExpNeq e1 e2)          = ic_interpret_binary_operation e1 e2 (||!=||)
ic_interpret_expression (ExpGt e1 e2)           = ic_interpret_binary_operation e1 e2 (||>||)
ic_interpret_expression (ExpGeq e1 e2)          = ic_interpret_binary_operation e1 e2 (||>=||)
ic_interpret_expression (ExpLt e1 e2)           = ic_interpret_binary_operation e1 e2 (||<||)
ic_interpret_expression (ExpLeq e1 e2)          = ic_interpret_binary_operation e1 e2 (||<=||)


ic_interpret_expression (ExpLIncr e)             = do
    
    -- overall value doesn't matter; just the reference...
    (_, r) <- ic_interpret_expression e

    case r of 
        ReferênciaNão           -> ic_raise $ "L-increment in lvalue"
        Referência varaccess    -> do
            -- for which the value is taken from the variable,
            v <- ic_pm_read varaccess

            -- incremented,
            let v' = v ||+|| (ValueInt 1)

            -- and the written again;
            ic_pm_write varaccess v'

            -- lincr return lvalue is the newer one.
            return $ (v', r)
    

ic_interpret_expression (ExpRIncr e)             = do
    (_, r) <- ic_interpret_expression e

    case r of 
        ReferênciaNão           -> ic_raise $ "R-increment in rvalue"
        Referência varaccess    -> do
            v <- ic_pm_read varaccess
            ic_pm_write varaccess (v ||+|| (ValueInt 1))

            -- rincr return lvalue is the older one.
            return $ (v, r)

ic_interpret_expression (ExpLDecr e)             = do ic_raise "dd" -- Shouldn't happen?? Semantics should do Decr -> Incr?
ic_interpret_expression (ExpRDecr e)             = do ic_raise "dd"


ic_interpret_expression (ExpFCall fname params) = do
    args <- mapM ic_interpret_expression params
    let largs = fst <$> args

    st      <- ic_get_st
    
    case Map.lookup fname st of
        Just function@(FuncDef _ _ _ _ _ _) -> do
            (\(v, _) -> (v, ReferênciaNão)) <$> (ic_interpret_function function largs)
        _               -> do
            -- agora tenta buscar na memória de programa pra ver se tem alguma função pra né.
            pm <- ic_get_pm
            case Map.lookup fname pm of
                Just (ValueFunction fname')  -> 
                    case Map.lookup fname' st of
                        Just function@(FuncDef _ _ _ _ _ _) -> do
                            (\(v, _) -> (v, ReferênciaNão)) <$> (ic_interpret_function function largs)

                _                       -> ic_raise $ "Invalid function " ++ show fname -- fim da linha zz

ic_interpret_expression (ExpFCall_Implicit fcall_exp params) = do ic_raise "ExpFCall_Implicit NYI"


ic_interpret_expression (ExpStructInstance sname exps) = do
    -- getting the lvalues.
    values <- mapM ic_interpret_expression exps 

    -- ic_io $ putStrLn $ "=== Instanciando estrutura: " ++ sname ++ "; valores = " ++ show values

    let lvalues = fst <$> values

    return $ (ValueStruct sname lvalues, ReferênciaNão)


ic_interpret_expression (ExpArrayInstancing exps) = do
    -- getting the lvalues.
    values <- mapM ic_interpret_expression exps 
    let lvalues = fst <$> values

    -- checking for compatibility on the types.
    -- ...

    return $ (ValueArray lvalues, ReferênciaNão)
    

ic_interpret_expression (ExpNew t) = do
    v <- to_value t
    return $ (v, ReferênciaNão)

ic_interpret_expression (ExpLambda rtype params captures cmds) = do ic_raise "Lambda not yet implemented."
ic_interpret_expression (ExpLiftedLambda sname) = do ic_raise "Lambda not yet implemented."
    

ic_interpret_binary_operation :: IR_Expression -> IR_Expression -> (Value -> Value -> Value) -> InterpreterContext (Value, Reference)
ic_interpret_binary_operation e1 e2 op  = do
    (v1, _) <- ic_interpret_expression e1
    (v2, _) <- ic_interpret_expression e2
    return $ (v1 `op` v2, ReferênciaNão) 

