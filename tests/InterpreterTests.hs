{-  -------------------------------
    @file       tests/InterpreterTests.hs
    @details    ...
-}

module InterpreterTests (tests) where

import Test.Hspec
import Interpreter.Interpreter
import ParserTests (fparse)

import Frontend.Error   

import Data.Either (fromRight)



fparse_and_interpret :: FilePath -> IO InterpreterResult
fparse_and_interpret filepath = do
    parsed <- fparse filepath

    case parsed of
        Left s      -> return $ Left s
        Right p     -> interpret p


asd :: FilePath -> IO (Value, ProgramLog)
asd filepath = do 
    result <- fparse_and_interpret filepath
    return $ get_rc_and_log $ result

-------------------------------



tests :: Spec
tests = describe "Parser tests" $ do
    error_specs
    sample_specs


error_specs :: Spec
error_specs = describe "Execution errors" $ do
    it "nothing" $ do
        -- @TODO
        1 `shouldBe` 1


sample_specs :: Spec
sample_specs = describe "Sample programs" $ do
    it "ex1.sl" $ do
        result <- asd "data/sl/ex1.sl"
        result `shouldBe` (ValueInt 0, ["INT: 120"])

    it "ex2.sl" $ do
        result <- asd "data/sl/ex2.sl"
        result `shouldBe` (ValueUnknown, [
            "STRING: \"Alice\"", "INT: 25", "FLOAT: 1.65",
            "STRING: \"Bob\"", "INT: 30", "FLOAT: 1.8",
            "STRING: \"Charlie\"", "INT: 35", "FLOAT: 1.75"
            ])

    it "ex3.sl" $ do
        result <- asd "data/sl/ex3.sl"
        result `shouldBe` (ValueUnknown, [
            "INT: 5", "INT: 4", "INT: 3", "INT: 2", "INT: 1"
            ])

    it "ex4.sl" $ do
        result <- asd "data/sl/ex4.sl"
        result `shouldBe` (ValueUnknown, [
            "FLOAT: 23.020408163265305",
            "BOOL: True",
            "STRING: \"condi\\231\\227o normal\""
            ])

    it "ex5.sl" $ do
        result <- asd "data/sl/ex5.sl"
        result `shouldBe` (ValueUnknown, [
            "FLOAT: 5.0",
            "INT: 5",
            "STRING: \"ol\\225\"",
            "FLOAT: 0.0",
            "FLOAT: 0.0",
            "FLOAT: 0.0",
            "FLOAT: 0.0",
            "FLOAT: 1.0"
            ])
