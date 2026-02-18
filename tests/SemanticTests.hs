{-  -------------------------------
    @file       tests/SemanticTests.hs
    @details    ...
-}

module SemanticTests (tests) where

import Test.Hspec
import ParserTests (fparse)

import Frontend.Error
import Frontend.IR
import Frontend.Semantics   
import Frontend.Pretty

import Data.Either (fromLeft)


--sl_verify :: IR_Program -> Either Error (IR_Program, SymbolTable)
verify_src :: FilePath -> IO (Either Error IR_Program)
verify_src filepath = do
    io_parsed <- fparse filepath
    return $ asd io_parsed where
        asd io_parsed = do
            parsed <- io_parsed
            (verified, _) <- sl_verify parsed
            return $ verified


verify_src_and_compare :: FilePath -> Either Error IR_Program -> IO ()
verify_src_and_compare filepath result = do
    verified <- verify_src filepath
    shouldBe verified result


se :: String -> (Int, Int) -> Either Error a 
se s p = Left $ Error SemanticalError s (SrcPos p)

sem :: [(String, (Int, Int))] -> Either Error a
sem ss  =   let errs = map (\(x, y) -> fromLeft undefined $ se x y) ss
            in  Left $ MultipleErrors errs


tests :: Spec
tests = describe "Semantical tests" $ do
    symbol_error_specs
    type_error_specs
    scope_error_specs
    lambda_specs
    global_specs


symbol_error_specs :: Spec
symbol_error_specs = describe "Statement violations" $ do
    it "Invalid structs (ep1.sl)" $ do
        verify_src_and_compare "data/sl/sa/ep1.sl" $ sem [
            ("Structure \"A\" have no fields!",                                             (7, 8)),
            ("Field \"x\" declared as void on struct \"camarada\"",                         (11, 8)),
            ("Invalid indices for array field \"a\" of base type A",                        (11, 8)),
            ("Invalid indices for array field \"b\" of base type A",                        (11, 8)),
            ("Invalid indices for array field \"c\" of base type A",                        (11, 8)),
            ("Invalid indices for array field \"e\" of base type A",                        (11, 8)),
            ("Field \"z\" declared as a function on struct \"asd\"",                        (22, 8))
            ]

    it "Multiple symbol definition (ep2.sl)" $ do
        verify_src_and_compare "data/sl/sa/ep2.sl" $ sem [
            ("Symbol \"A\" defined multiple (3) times",                                     (19, 8)),
            ("Symbol \"asd\" defined multiple (2) times",                                   (11, 6))
            ]
        

type_error_specs :: Spec
type_error_specs = describe "Type violations" $ do
    it "No-explicit return (ep3.sl)" $ do
        verify_src_and_compare "data/sl/sa/ep3.sl" $ sem [
            ("Function \"doidera\" have no explicit return but expects float",              (7, 6)),
            ("Function \"main\" have no explicit return but expects int",                   (13, 6)),
            ("Variable \"z\" (int) is not being used on function \"main\"",                 (16, 34))
            ]
    
    it "Unexpected return type (ep4.sl)" $ do
        verify_src_and_compare "data/sl/sa/ep4.sl" $ sem [
            ("Function \"coisa_doida\" expects type void; yet, int is returned",            (8, 5)),
            ("Function \"negocio_maluco\" expects type int; yet, string is returned",       (12, 5)),
            ("Function \"objeto_delirado\" expects type void; yet, int is returned",        (16, 5))
            ]

    it "Invalid attribution (ep6.sl)" $ do
        verify_src_and_compare "data/sl/sa/ep6.sl" $ sem [
            ("Variable \"y\" (float) is set to an expression that evaluates to int",        (30, 22)),
            ("Assigning expression of type string to \"x\" (int)",                          (32, 39)),
            ("Invalid expression result of float * string",                                 (33, 26)),
            ("Assigning expression of type float to \"h.x\" (int)",                         (40, 14)),
            ("Assigning expression of type int[2] to \"h.y\" (A)",                          (42, 17)),
            ("Assigning expression of type string to \"h.y\" (A)",                          (43, 38)),
            ("Assigning expression of type float to \"h.y\" (A)",                           (44, 14)),
            ("Indexing non-array (int) at [0] on variable \"h\"",                           (46, 23))
            ]

    it "Arrays, structs and functions (ep8.sl)" $ do
        verify_src_and_compare "data/sl/sa/ep8.sl" $ sem [
            ("Null-array instancing. What about a no?",                                             (46, 25)),
            ("Non-homogeneous array instancing: [int, float, string]",                              (47, 41)),
            ("Variable \"z3\" (int[]) is set to an expression that evaluates to float[3]",          (48, 38)),
            ("Invalid array instancing: new int[]",                                                 (50, 32)),
            ("Allocating non-array type int is meaningless",                                        (51, 30)),

            ("Variable \"z5\" (int[]) is not being used on function \"arrays\"",                    (51, 30)),
            ("Variable \"z4\" (int[]) is not being used on function \"arrays\"",                    (50, 32)),
            ("Variable \"z3\" (int[]) is not being used on function \"arrays\"",                    (48, 38)),
            ("Variable \"z2\" (int[]) is not being used on function \"arrays\"",                    (47, 41)),
            ("Variable \"z\" (int[]) is not being used on function \"arrays\"",                     (46, 25)),
            ("Variable \"y\" (int[]) is not being used on function \"arrays\"",                     (43, 30)),
            ("Variable \"x\" (int[]) is not being used on function \"arrays\"",                     (42, 35)),
            ("Variable \"w\" (int[4]) is not being used on function \"arrays\"",                    (41, 34)),

            ("Calling struct \"Aluno\" (defined at (7, 8)) as a function!",                         (60, 22)),
            ("Variable \"b\" (Aluno) is not being used on function \"structs\"",                    (58, 25)),
            ("Variable \"a\" (Aluno) is not being used on function \"structs\"",                    (57, 31)),
            ("Function \"function_not_defined\" is undefined",                                      (67, 34)),
            ("Calling function \"SOMA\" with wrong number of arguments (3 out of 2)",               (70, 49)),
            ("Invalid argument #1 on function \"SOMA\" call: expected int, yet, argument is float", (73, 33)),
            ("Invalid argument #2 on function \"SOMA\" call: expected int, yet, argument is float", (73, 33)),
            ("Variable \"x\" (int), first defined at (70, 49), is redefined with type int",         (73, 33)),
            ("Variable \"x\" (int) is not being used on function \"functions\"",                    (70, 49))
            ]

scope_error_specs :: Spec
scope_error_specs = describe "Scope violations" $ do
    it "Variable re-definition (ep5.sl)" $ do
        verify_src_and_compare "data/sl/sa/ep5.sl" $ sem [
            ("Variable \"x\" (int), first defined at (7, 6), is redefined with type int",   (7, 6)),
            ("Variable \"x\" (int), first defined at (7, 6), is redefined with type int",  (8, 14)),
            ("Variable \"x\" (int), first defined at (7, 6), is redefined with type int",   (9, 20)),
            ("Variable \"x\" (int), first defined at (7, 6), is redefined with type float", (10, 24)),
            ("Function \"tudo_certo\" expects type int; yet, string is returned",           (13, 5))
            ]

    it "Access violation (ep7.sl)" $ do
        -- verified <- verify_src "data/sl/sa/ep7.sl"
        -- putStrLn $ pretty_sl verified

        verify_src_and_compare "data/sl/sa/ep7.sl" $ sem [
            ("Invalid field \"campo\" access of structure B on variable \"h\"",             (43, 19)),
            ("Invalid field \"campo_todo\" access of structure B on variable \"h\"",        (44, 24)),
            ("Invalid field \"campo_todo_errado\" access of structure B on variable \"h\"", (45, 31)),
            ("Invalid field \"size\" access of structure B on variable \"h\"",              (46, 18)),
            ("Variable \"H\" is not defined",                                               (49, 10)),
            ("Variable \"this_one\" is not defined",                                        (50, 35)),
            ("Variable \"is_also\" is not defined",                                         (50, 35)),
            ("Variable \"undefined\" is not defined",                                       (50, 35)),
            --("Variable \"B\" is not defined",                                               (53, 14)),
            --("Variable \"B\" is not defined",                                               (53, 14)),
            ("Invalid \"camarada\" field access on array on variable \"h\"",                (56, 36)),
            ("Invalid array access on variable \"h\"",                                      (57, 36)),
            ("Invalid access of \"coisa_doida\" on type int on variable \"h\"",             (58, 36))
            ]


lambda_specs :: Spec
lambda_specs = describe "Lambda" $ do
    
    it "Errors #1 (ep9.sl)" $ do
        verify_src_and_compare "data/sl/sa/ep9.sl" $ sem [
            ("Calling function \"lambda\" with wrong number of arguments (3 out of 1)",     (45, 6)),
            ("Calling function \"f\" with wrong number of arguments (3 out of 6)",          (49, 15)),
            ("Calling function \"f\" with wrong number of arguments (4 out of 6)",          (50, 16)),
            ("Calling function \"f\" with wrong number of arguments (5 out of 6)",          (51, 19)),
            ("Calling function \"f\" with wrong number of arguments (7 out of 6)",          (52, 30)),
            ("Invalid argument #1 on function \"f\" call: expected int, yet, argument is float",          (55, 35))
            ]

    {-
    it "Lifting (rp1.sl)" $ do
        verified <- verify_src "data/sl/sa/rp1.sl"
        putStrLn $ pretty_sl verified
        1 `shouldBe` 1
        verify_src_and_compare "data/sl/sa/rp1.sl" $ sem [
            ("Invalid field \"campo\" access of structure B on variable \"h\"",             (43, 19)),
            ("Invalid field \"campo\" access of structure B on variable \"h\"",             (43, 19))
            ]
    -}


global_specs :: Spec
global_specs = describe "General program structure" $ do
    it "Empty translation unit" $ do
        -- @TODO
        1 `shouldBe` 1

    it "No-driver" $ do
        -- @TODO
        1 `shouldBe` 1

    it "Data-structure example (ex7.sl)" $ do
        verified <- verify_src "data/sl/ex7.sl"
        putStrLn $ pretty_sl verified
        1 `shouldBe` 1
