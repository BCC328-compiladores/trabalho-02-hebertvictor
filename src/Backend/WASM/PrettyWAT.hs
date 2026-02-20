{-  -------------------------------------
    @file       src/Backend/WASM/PrettyWAT.hs
    @details    Sobrecarrega pretty pra WASM_Code pra terminar de compilar o código né pai.
-}

module Backend.WASM.PrettyWAT where

import Frontend.Pretty
import Frontend.Parser (parse_sl)
import Frontend.Semantics
import Frontend.IR
import Frontend.Error

import Backend.WASM.WAT_Codegen
import Data.Either
import Data.Map


-- Parsing the program from a file directly.
_parse :: String -> IO IR_Program
_parse textcode = do
    let asd = Data.Either.fromRight (Program []) $ parse_sl textcode
    return $ asd

_fdoidera :: String -> IO SymbolTable
_fdoidera textcode = do
    parsed <- _parse textcode
    let asd = snd $ Data.Either.fromRight (Program [], Data.Map.empty) $ (sl_verify parsed)
    return $ asd

asd = do 
    st <- _fdoidera "func add(x: int, y: int) : int { return x + y; }"
    putStrLn $ pretty_sl $ sl_to_wat st


{-  Exemplo mínimo:
(module
  (func (export "add") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.add
  )
)
-}

pc_scope :: PrettyContext ()
pc_scope = do
    pc_increment_identation
    pc_newline

pc_descope = do
    pc_decrement_identation
    pc_newline


print_instructions :: [WASM_Instruction] -> PrettyContext ()
print_instructions = pc_list_wsep pc_newline


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
    pretty (WF fname params result body) = do
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

        pc_decrement_identation
        pc_newline
        pc_tell ")"


instance Pretty WASM_Type where
    pretty I32 = pc_tell "i32"
    pretty I64 = pc_tell "i64"
    pretty F32 = pc_tell "f32"
    pretty F64 = pc_tell "f64"


instance Pretty WASM_Instruction where
    -- I32 data type.
    pretty (I32_Const x)        = pretty I32 >> pc_tell ".const " >> pc_tell (show x)
    pretty I32_Add              = pretty I32 >> pc_tell ".add" 
    pretty I32_Sub              = pretty I32 >> pc_tell ".sub"
    pretty I32_Mul              = pretty I32 >> pc_tell ".mul"
    pretty I32_DivS             = pretty I32 >> pc_tell ".div_s"
    pretty I32_DivU             = pretty I32 >> pc_tell ".div_u"
    pretty I32_RemS             = pretty I32 >> pc_tell ".rem_s"
    pretty I32_RemU             = pretty I32 >> pc_tell ".rem_u"
    pretty I32_And              = pretty I32 >> pc_tell ".and"
    pretty I32_Or               = pretty I32 >> pc_tell ".or"
    pretty I32_Xor              = pretty I32 >> pc_tell ".xor"
    pretty I32_Shl              = pretty I32 >> pc_tell ".shl"
    pretty I32_ShrS             = pretty I32 >> pc_tell ".shr_s"
    pretty I32_ShrU             = pretty I32 >> pc_tell ".shr_u"
    pretty I32_Rotl             = pretty I32 >> pc_tell ".rotl"
    pretty I32_Rotr             = pretty I32 >> pc_tell ".rotr"

    pretty I32_Eq               = pretty I32 >> pc_tell ".eq"
    pretty I32_Ne               = pretty I32 >> pc_tell ".ne"
    pretty I32_LtS              = pretty I32 >> pc_tell ".lt_s"
    pretty I32_GtS              = pretty I32 >> pc_tell ".gt_s"
    pretty I32_LtU              = pretty I32 >> pc_tell ".lt_u"
    pretty I32_GtU              = pretty I32 >> pc_tell ".gt_u"
    
    pretty I32_LeS              = pretty I32 >> pc_tell ".le_s"
    pretty I32_GeS              = pretty I32 >> pc_tell ".ge_s"
    pretty I32_LeU              = pretty I32 >> pc_tell".le_u"
    pretty I32_GeU              = pretty I32 >> pc_tell ".ge_u"

    -- Stack control.
    pretty (LocalDecl sname wtype)= do 
        pc_tell "(local $"
        pretty sname
        pc_tell " "
        pretty wtype 
        pc_tell ")"

    pretty (LocalGet sname)     = pc_tell "local.get $" >> pretty sname
    pretty (LocalSet sname)     = pc_tell "local.set $" >> pretty sname
    pretty Drop                 = pc_tell "drop"
    
    -- Function & scope.
    pretty (Call sname)         = pc_tell "call $" >> pretty sname
    pretty WReturn              = pc_tell "return"
    pretty (Block label instructions) = do
        pc_newline -- <- só pra organizar né pai...
        pc_tell "(block $"
        pretty label
        
        pc_scope
        print_instructions instructions
        pc_descope
        pc_tell ")"

    -- essentially a block...
    pretty (Loop label instructions) = pretty (Block label instructions)

    -- Control-flux & jumps.
    pretty (Br label) = do
        pc_tell "br $"
        pretty label

    pretty (BrIf label) = do
        pc_tell "br_if $"
        pretty label
        pc_newline -- (pra organizar)

    pretty (WIf result if_instructions else_instructions) = do
        pc_tell "(if"

        case result of 
            Nothing -> do
                return $ ()

            Just t -> do
                pc_tell " (result "
                pretty t
                pc_tell ")"

        pc_scope

        -- THEN.
        pc_tell "(then"
        pc_scope
        print_instructions if_instructions
        pc_descope
        pc_tell ")"
        pc_newline

        -- ELSE é opcional.
        case else_instructions of
            [] -> return $ ()

            _ -> do
                pc_tell "(else"
                pc_scope
                print_instructions else_instructions
                pc_descope
                pc_tell ")"



example_minimal :: WASM_Code
example_minimal =
  WASM_Code
    [ WF
        { wf_name   = "seven"
        , wf_params = []
        , wf_result = Just I32
        , wf_body   =
            [ I32_Const 7
            ]
        }
    ]


example_add :: WASM_Code
example_add =
  WASM_Code
    [ WF
        { wf_name   = "add"
        , wf_params = [I32, I32]
        , wf_result = Just I32
        , wf_body   =
            [ LocalGet "x"
            , LocalGet "y"
            , I32_Add
            ]
        }
    ]

example_minimal_str = pretty_sl example_minimal


example_block :: WASM_Code
example_block =
  WASM_Code
    [ WF
        { wf_name   = "blockExample"
        , wf_params = []
        , wf_result = Just I32
        , wf_body   =
            [ Block "exit"
                [ I32_Const 42
                , Br "exit"
                , I32_Const 0
                ]
            ]
        }
    ]

example_if :: WASM_Code
example_if =
  WASM_Code
    [ WF
        { wf_name   = "isPositive"
        , wf_params = [I32]
        , wf_result = Just I32
        , wf_body   =
            [ LocalGet "x"           -- parâmetro
            , I32_Const 0
            , I32_GtS                -- x > 0
            , WIf (Just I32)         -- tipo do resultado do if
                [ I32_Const 1 ]      -- then
                [ I32_Const 0 ]      -- else
            ]
        }
    ]


example_loop :: WASM_Code
example_loop =
  WASM_Code
    [ WF
        { wf_name   = "countdown"
        , wf_params = []
        , wf_result = Just I32
        , wf_body   =
            [ LocalDecl "i" I32
            , I32_Const 3
            , LocalSet "i"
        
            , Block "exit"
                [ Loop "top"
                    [ LocalGet "i"
                    , I32_Const 0
                    , I32_Eq
                    , BrIf "exit"
                    , LocalGet "i"
                    , I32_Const 1
                    , I32_Sub
                    , LocalSet "i"
                    , Br "top"
                    ]
                ]

            , LocalGet "i"
            ]
        }
    ]
