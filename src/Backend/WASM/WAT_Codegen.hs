{-  -------------------------------------
    @file       src/Backend/WASM/WAT_Codegen.hs
    @details    Compiles the IR into a WAT IR that can nextly be converted into WAT via pretty...
-}

module Backend.WASM.WAT_Codegen where

import Frontend.Pretty
import Frontend.IR
import Frontend.Error


import qualified Data.Map as Map


---------
-- API --
---------



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
        wf_locals   :: [WASM_Type],  -- @TODO WTF N ENTENDI NADA
        wf_body     :: [WASM_Instruction]
    }
    deriving (Eq, Show)


data WASM_Type =
    I32 |
    I64 |
    F32 |
    F64 
    deriving (Eq, Show)


to_wasm_type :: IR_Type -> Maybe WASM_Type
to_wasm_type TypeInt   = Just I32
to_wasm_type TypeBool  = Just I32
to_wasm_type TypeFloat = Just F32
to_wasm_type TypeVoid  = Nothing     -- Nothing will mean no type
to_wasm_type _         = error "Unsupported..."


data WASM_Instruction = 
    I32Const    Int |
    I32Add |
    I32Sub |
    I32Mul |
    I32Div | 
    
    LocalGet    Identifier |
    LocalSet    Identifier |
    
    Call        Identifier |
    Return | 
    
    If          (Maybe WASM_Type) [WASM_Instruction] [WASM_Instruction] |
    Block       Identifier [WASM_Instruction] | 
    Loop        Identifier [WASM_Instruction] | 
    Br          Identifier |
    BrIf        Identifier |
    
    Drop
  deriving (Eq, Show)


{-

translate_program :: IR_Program -> WASM_Code
translate_program (Program stmts) = translate_statements stmts


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



get_var_wasm_type :: IR_Var -> WASM_Type
get_var_wasm_type (VarDecl _ t) = to_wasm_type t

sc_initial_cgstate :: [IR_Var] -> CodegenState
sc_initial_cgstate vars =
    CodegenState Map.empty (map $ get_var_wasm_type vars) 0 [] 0



---------------------
-- Code Generation --
---------------------

-- translate_statement :: IR_Statement -> WASM_Function
-- translate_statement (FuncDef fname rtype params _ body _) = do
--     let initialState = sc_initial_cgstate $ params
-}