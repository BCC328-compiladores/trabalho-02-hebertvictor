{-  -------------------------------
    @file       tests/InterpreterTests.hs
    @details    ...
-}

module InterpreterTests (tests) where

import Test.Hspec
import Interpreter.Interpreter
import ParserTests (fparse)

import Frontend.IR
import Frontend.Error
import Frontend.Pretty
import Frontend.Parser ( parse_sl )
import Frontend.Semantics (sl_verify)

import Data.Either (fromRight)



fparse_and_interpret :: FilePath -> IO InterpreterResult
fparse_and_interpret filepath = do
    parsed <- fparse filepath
    
    case parsed of
        Left s      -> return $ Left s
        Right p     -> do
            let verified = sl_verify p

            case verified of 
                Left s -> do
                    return $ Left s

                Right (p', _) -> do 
                    interpret p'


asd :: FilePath -> IO (Value, ProgramLog)
asd filepath = do 
    result <- fparse_and_interpret filepath
    return $ get_rc_and_log $ result

-------------------------------



tests :: Spec
tests = describe "Interpreter tests" $ do
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

    it "ex7.sl" $ do
        result <- asd "data/sl/ex7.sl"
        result `shouldBe` (ValueInt 0, ["INT: 1", "INT: 4"])


-------------
-- Unit tests --
-------------

-- @TODO create unit test to them...

p1 = parse_sl "func SOMA(x: int, y: int) : int { return x + y; }\nfunc main(void) : void { let x : int = 1; let y : int = 2; let z : int = SOMA(x, y) ** 5; return; }"
p2 = parse_sl "func main(argsc: int, argsv : string[]) : int { \n let x : int = 1; if (x > 5) { x = 10; } else { x = 11; }\n return x; }"
p3 = parse_sl "func main() : int { let x : int = 7; let y : int = 3; let a : float = 2.5; let b : float = 2.0; let z : int = x / y; return z; }"
p4 = parse_sl "func main(argsc: int, argsv : string[]) : int { \n let x : int = 1; while (x < 5) { ++ x; }\nreturn x ++; }"
p5 = parse_sl "func asd(void) { }\nfunc asd(void) { }\nfunc main(argsc: int, argsv : string[]) : int { \n let x : int = 1; while (x < 5) { ++ x; }\nreturn x ++; }"
p6 = parse_sl "struct A { } struct camarada { x: void; y: A; z1: A[1]; z2: A[][-1][5][\"ok\"]; }\nfunc asd(void) { return x + y; }"
p7 = parse_sl "struct A { x: int[2][1 + 1]; }func SOMA(x: int, y: int) : int { return x + y; }\nfunc main(void) : int { let x : int = 1; let y : int = 2; let z : int = SOMA(x, y) ** 5; }"
p8 = parse_sl "func tudo_certo(x : int, x: int) : int { let x = 1; let x : int = 2; let x : float = 3.0; return \"ok\"; }" -- problematic
p9 = parse_sl "struct A { x: int[2][2]; } struct B { x: int; y : A; } func acesso_errado(void) { let x : int = 1; let y : float = x; }"

pp1 = Data.Either.fromRight (Program []) p1
pp2 = Data.Either.fromRight (Program []) p2
pp3 = Data.Either.fromRight (Program []) p3
pp4 = Data.Either.fromRight (Program []) p4
pp5 = Data.Either.fromRight (Program []) p5
pp6 = Data.Either.fromRight (Program []) p6
pp7 = Data.Either.fromRight (Program []) p7
pp8 = Data.Either.fromRight (Program []) p8
pp9 = Data.Either.fromRight (Program []) p9

r1 = interpret pp1
r2 = interpret pp2
r3 = interpret pp3
r4 = interpret pp4
r5 = interpret pp5


-- rapidão
present_semantic (Left err)      = Left err
present_semantic (Right (p, _))  = Right p

s1 = putStrLn $ pretty_sl $ present_semantic $ sl_verify pp1
s5 = putStrLn $ pretty_sl $ present_semantic $ sl_verify pp5
s6 = putStrLn $ pretty_sl $ present_semantic $ sl_verify pp6
s7 = putStrLn $ pretty_sl $ present_semantic $ sl_verify pp7
s8 = putStrLn $ pretty_sl $ present_semantic $ sl_verify pp8
s9 = putStrLn $ pretty_sl $ present_semantic $ sl_verify pp9