{-  -------------------------------------
    @file       src/Backend/WASM/PrettyWAT.hs
    @details    Sobrecarrega pretty pra WASM_Code pra terminar de compilar o código né pai.
-}

module Backend.WASM.PrettyWAT where

import Frontend.Pretty
import qualified Frontend.IR as IR
import Frontend.Error

import Backend.WASM.WAT_Codegen


{-  Exemplo mínimo:
(module
  (func (export "add") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.add
  )
)
-}

instance Pretty WASM_Code where
    pretty (WASM_Code functions) = do
        pc_tell "(module"
        pc_increment_identation
        pc_newline

        -- packing functions...
        pc_list_wsep (pc_newline >> pc_newline) functions

        pc_decrement_identation
        pc_newline
        pc_tell ")"


instance Pretty WASM_Function where
    pretty (WF fname params result locals body) = do
        pc_tell "(func (export "
        pc_tell $ show fname
        pc_tell ") "
        
        -- parameters
        case params of 
            []  -> do 
                return $ ()

            _ -> do
                pc_tell "(param "
                pc_list_wsep (pc_tell " ") params
                pc_tell ") "
        
        case result of
            Nothing -> do
                return $ ()

            Just rtype -> do
                pc_tell "(result "
                pretty rtype
                pc_tell ")"

        pc_increment_identation
        pc_newline

        pc_list_wsep pc_newline body

        pc_tell ")"


instance Pretty WASM_Type where
    pretty I32 = pc_tell "i32"
    pretty I64 = pc_tell "i64"
    pretty F32 = pc_tell "f32"
    pretty F64 = pc_tell "f64"


instance Pretty WASM_Instruction where
    pretty (I32Const x)         = pc_tell $ show x
    pretty I32Add               = pretty I32 >> pc_tell ".add" 
    pretty I32Sub               = pretty I32 >> pc_tell ".sub"
    pretty I32Mul               = pretty I32 >> pc_tell ".mul"
    pretty I32Div               = pretty I32 >> pc_tell ".div"

    pretty (LocalGet sname)     = pc_tell "local.get $" >> pretty sname
    pretty (LocalSet sname)     = pc_tell "local.set $" >> pretty sname
    
    pretty (Call sname)         = pc_tell "call $" >> pretty sname
    pretty Return               = pc_tell "return"

    pretty (Block label instructions) = do
        pc_tell "(block $"
        pretty label
        
        pc_increment_identation
        pc_newline
        
        pc_list_wsep pc_newline instructions

        pc_decrement_identation
        pc_newline
        pc_tell ")"

    -- essentially a block...
    pretty (Loop label instructions) = pretty (Block label instructions)

    pretty (Br label) = do
        pc_tell "br "
        pretty label



example_minimal :: WASM_Code
example_minimal =
  WASM_Code
    [ WF
        { wf_name   = "seven"
        , wf_params = []
        , wf_result = Just I32
        , wf_locals = []
        , wf_body   =
            [ I32Const 7
            ]
        }
    ]
