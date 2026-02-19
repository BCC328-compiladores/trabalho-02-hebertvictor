{-  -------------------------------------
    @file       src/Backend/WASM/Codegen.hs
    @details    ~
-}

import qualified Data.Map as Map
import control


---------
-- API --
---------

compile_program :: IR_Program -> WASM_Code
compile_program (Program stmts) = map 

convert_code :: WASM_Code -> String
convert_code wcode = 


--------------
-- WASM IR ---
--------------

data WASM_Type =
    I32 |
    I64 |
    F32 |
    F64 
    deriving (Eq, Show)

ir_type_to_wasm :: IR_Type -> Maybe WASM_Type
ir_type_to_wasm TypeInt   = Just I32
ir_type_to_wasm TypeBool  = Just I32
ir_type_to_wasm TypeFloat = Just F32
ir_type_to_wasm TypeVoid  = Nothing     -- Nothing will mean no type
ir_type_to_wasm _         = error "Unsupported..."

data WASM_Instruction = 
    I32Const Int |
    I32Add |
    I32Sub |
    I32Mul |
    I32Div | 
    
    LocalGet Int |
    LocalSet Int |
    
    Call String | 
    Return | 
    
    If (Maybe WASM_Type) [WASM_Instruction] [WASM_Instruction] |
    Block [WASM_Instruction] | 
    Loop [WASM_Instruction] | 
    Br Int | 
    BrIf Int | 
    
    Drop
  deriving (Eq, Show)

data WASM_Function = 
    WF { 
        wasm_fname    :: String, 
        wasm_fparams  :: [WASM_Type], 
        wasm_fresult  :: Maybe WASM_Type, 
        wasm_flocals  :: [WASM_Type], 
        wasm_fbody    :: [WASM_Instruction]
    }
    deriving (Eq, Show)

-- The WASM code resepresentaion as a data strucutre
data WASM_Code = [WASM_Function]

data LocalsMap = Map Identifier Int

data CodegenState = 
    CodegenState { 
        cs_locals       :: LocalsMap,           -- defines the current local enviroment in the WASM context.
        cs_local_types  :: [WASM_Type],         -- types for the current locals
        cs_next_local   :: Int,                 -- counter (index where to allocate next local)
        cs_inst         :: [WASM_Instruction],  -- accumulated WASM instructions generated list
        cs_label_depth  :: Int                  -- controls the block's depth to branch operations
    }


newtype CodegenContext v = CC {
    codegen_context_run :: CodegenState -> (v, CodegenState)
}


---------------
-- Instances --
---------------

instance Functor CodegenContext where
    fmap f (CC transition) =
        CC $ \state ->
            let (value, state') = transition state
            in (f value, state')

instance Applicative CodegenContext where
    pure x = CC $ \state -> (x, state)

    (CC transition_f) <*> (CC transition_x) = CC $ \state ->
        -- evaluating the function;
        let (f, state_f) = transition_f state

        -- evaluating the value;
            (x, state_x) = transition_x state_f
        
        -- compositing f with x.
        in (f x, state_x)

instance Monad CodegenContext where
    return = pure

    (CC transition) >>= k = CC $ \state ->
        -- executes the first action
        let (value, state') = transition state

        -- gets the seconds and then evaluates it,
            CC transition' = k value
            (value', state'') = transition' state'
        
        -- and returns the final value ans state.
        in (value', state'')


-- getters and setters.
cc_get_locals :: CodegenContext LocalsMap
cc_get_locals = CC $ \state -> (cs_locals state, state)

cc_get_local_types :: CodegenContext [WASM_Type]
cc_get_local_types = CC $ \state -> (cs_local_types state, state)

cc_get_next_locals :: CodegenContext Int
cc_get_next_locals = CC $ \state -> (cs_next_locals state, state)

cc_get_inst :: CodegenContext [WASM_Instruction]
cc_get_inst = CC $ \state -> (cs_inst state, state)

cc_get_label_depth :: CodegenContext Int
cc_get_label_depth = CC $ \state -> (cs_label_depth state, state)



sc_set_locals :: LocalsMap -> CodegenContext ()
sc_set_locals l = CC $ \(CodegenState _ lt nl insts ld) -> ((), CodegenState l lt nl insts ld)

sc_set_local_types :: [WASM_Type] -> CodegenContext ()
sc_set_local_types lt = CC $ \(CodegenState l _ nl insts ld) -> ((), CodegenState l lt nl insts ld)

sc_set_next_locals :: Int -> CodegenContext ()
sc_set_next_locals nl = CC $ \(CodegenState l lt _ insts ld) -> ((), CodegenState l lt nl insts ld)

sc_set_inst :: [WASM_Instruction] -> CodegenContext ()
sc_set_inst insts = CC $ \(CodegenState l lt nl _ ld) -> ((), CodegenState l lt nl insts ld)

sc_set_label_depth :: Int -> CodegenContext ()
sc_set_label_depth ld = CC $ \(CodegenState l lt nl insts _) -> ((), CodegenState l lt nl insts ld)



sc_empty_cgstate :: [IR_Var] -> CodegenState
sc_empty_cgstate vars = 



---------------------
-- Code Generation --
---------------------

compile_statement :: IR_Statement -> WASM_Function
compile_statement (FuncDef fname rtype params _ body _) = do
    let initialState = sc_empty_cgstate $ params
        ((), finalState) = codegen_context_run (compile_function_body body) initialState
    in buildWasmFunction name rtype params finalState

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
    return $ Program statements'


genExpr :: Expr -> VarEnv -> String
genExpr (Lit n) _ =
    " i32.const " ++ show n
    
genExpr (Var x) env =
    case lookup x env of
        Just idx -> " local.get $" ++ x
        Nothing -> error $ "Variable not found: " ++ x