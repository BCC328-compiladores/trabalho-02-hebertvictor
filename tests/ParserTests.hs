{-  -------------------------------
    @file       tests/ParserTests.hs
    @details    testing the Parser...
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module ParserTests (tests, fparse) where

import Test.Hspec
import Frontend.Error
import Frontend.Parser
import Frontend.IR
import Frontend.IR (IR_Expression(ExpLitBoolean, ExpLitInteger))


tests :: Spec
tests = describe "Parser tests" $ do
    error_specs
    structure_specs
    simple_specs
    expr_specs
    cflx_specs
    array_specs
    program_specs


pe :: String -> (Int, Int) -> Either Error a
pe s p = Left $ Error SyntaxError s (SrcPos p)


error_specs :: Spec
error_specs = describe "Parsing errors" $ do
    it "generics error 1" $ do -- numbers won't go into the identifier list.
        let parsed = parse_sl "forall a b 1 . func doidera(x: a) : void { let y: b; let z: c; }"
        parsed `shouldBe` (pe "Lexeme: i1." (1, 12))

    it "generics error 2" $ do -- identifiers won't come after dot.
        let parsed = parse_sl "forall a b . c func doidera(x: a) : void { let y: b; let z: c; }"
        parsed `shouldBe` (pe "Lexeme: id \"c\"." (1, 14))

    it "generics error 3" $ do -- no types at all.
        let parsed = parse_sl "forall . func doidera(x: a) : void { let y: b; let z: c; }"
        parsed `shouldBe` (pe "Lexeme: punctuation \".\"." (1, 8))

    it "param list error 1" $ do
        let parsed = parse_sl "func doidera(x: int, ) : void { }"
        parsed `shouldBe` (pe "Lexeme: enc. \")\"." (1, 22))

    it "arg list error 1" $ do
        let parsed = parse_sl_exp "funcao(1, )"
        parsed `shouldBe` (pe "Lexeme: enc. \")\"." (1, 11))

    it "arg list error 2" $ do
        let parsed = parse_sl_exp "funcao(1, 2, )"
        parsed `shouldBe` (pe "Lexeme: enc. \")\"." (1, 14))

    it "arg list error 3" $ do
        let parsed = parse_sl_exp "funcao(void, 1)"
        parsed `shouldBe` (pe "Lexeme: kw void." (1, 8))


structure_specs :: Spec
structure_specs = describe "Parsing struct def" $ do
    it "empty" $ do
        let parsed = parse_sl "struct doidera { }"
        parsed `shouldBe` (Right $ Program [StructDef { symbol_name = "doidera", fields = [], symbol_pos = SrcPos (1, 8) }])
    
    it "empty2" $ do
        let parsed = parse_sl "struct doidera {\n\n}"
        parsed `shouldBe` (Right $ Program [StructDef { symbol_name = "doidera", fields = [], symbol_pos = SrcPos (1, 8) }])

    it "one field" $ do
        let parsed = parse_sl "struct doidera { x: int; }"
        parsed `shouldBe` (Right $ Program [StructDef { symbol_name = "doidera", fields = [VarDecl "x" TypeInt], symbol_pos = SrcPos (1, 8) }])
    
    it "one field2 error" $ do
        -- same thing, but this one should return an error (expression without semicolon).
        let parsed = parse_sl "struct doidera { x: int }"
        parsed `shouldBe` (pe "Lexeme: enc. \"}\"." (1, 25))

    it "student" $ do
        let parsed = parse_sl "struct aluno { nome: string; matricula: int; idade: int; peso: float; coeff: float; }"
        parsed `shouldBe` (Right $ Program [StructDef { 
            symbol_name = "aluno", 
            fields = [  VarDecl "nome" TypeString,
                        VarDecl "matricula" TypeInt,
                        VarDecl "idade" TypeInt,
                        VarDecl "peso" TypeFloat,
                        VarDecl "coeff" TypeFloat],
            symbol_pos = SrcPos (1, 8)}]
            )


simple_specs :: Spec
simple_specs = describe "Parsing simple functions" $ do
    it "empty" $ do
        let parsed = parse_sl "func camarada() : void {}"
        parsed `shouldBe` (Right $ Program [FuncDef {
            symbol_name         = "camarada",
            function_rtype      = TypeVoid,
            function_parameters = [],
            function_gtypes     = [],
            function_body       = [],
            symbol_pos          = SrcPos (1, 6)
        }])
    
    it "empty2" $ do
        let parsed = parse_sl "func camarada(void) : void {}"
        parsed `shouldBe` (Right $ Program [FuncDef {
            symbol_name         = "camarada",
            function_rtype      = TypeVoid,
            function_parameters = [],
            function_gtypes     = [],
            function_body       = [],
            symbol_pos          = SrcPos (1, 6)
        }])

    it "empty with let1" $ do
        let parsed = parse_sl "func camarada() : void { let x : int = 1; }"
        parsed `shouldBe` (Right $ Program [FuncDef {
            symbol_name         = "camarada",
            function_rtype      = TypeVoid,
            function_parameters = [],
            function_gtypes     = [],
            function_body       = [
                LC (VarDef (VarDecl "x" TypeInt) (ExpLitInteger 1)) (SrcPos (1, 41))
            ],
            symbol_pos          = SrcPos (1, 6)
        }])

    it "empty with let2" $ do
        let parsed = parse_sl "func camarada() : void { let x = 1; }"
        parsed `shouldBe` (Right $ Program [FuncDef {
            symbol_name         = "camarada",
            function_rtype      = TypeVoid,
            function_parameters = [],
            function_gtypes     = [],
            function_body       = [
                LC (VarDef (VarDecl "x" TypeVoid) (ExpLitInteger 1)) (SrcPos (1, 35))
            ],
            symbol_pos          = SrcPos (1, 6)
        }])

    it "empty paramaters 1" $ do
        let parsed = parse_sl "func A(a: int , b: int, c: int, d: int) : void { }"
        parsed `shouldBe` (Right $ Program [FuncDef {
            symbol_name         = "A",
            function_rtype      = TypeVoid,
            function_parameters = [VarDecl "a" TypeInt, VarDecl "b" TypeInt, VarDecl "c" TypeInt, VarDecl "d" TypeInt],
            function_gtypes     = [],
            function_body       = [],
            symbol_pos          = SrcPos (1, 6)
        }])

    it "generics 1" $ do
        let parsed = parse_sl "forall a . func doidera(x: a) : void { let y: a; }"
        parsed `shouldBe` (Right $ Program [FuncDef {
            symbol_name         = "doidera",
            function_rtype      = TypeVoid,
            function_parameters = [ VarDecl "x" (TypeGeneric "a") ],
            function_gtypes     = [ "a" ],
            function_body       = [
                LC (VarDef (VarDecl "y" (TypeGeneric "a")) ExpNothing) (SrcPos (1, 48))
            ],
            symbol_pos          = SrcPos (1, 17)
        }])

    it "generics 2" $ do
        let parsed = parse_sl "forall a b c . func doidera(x: a) : void { let y: b; let z: c; }"
        parsed `shouldBe` (Right $ Program [FuncDef {
            symbol_name         = "doidera",
            function_rtype      = TypeVoid,
            function_parameters = [ VarDecl "x" (TypeGeneric "a") ],
            function_gtypes     = [ "a", "b", "c" ],
            function_body       = [
                LC (VarDef (VarDecl "y" (TypeGeneric "b")) ExpNothing) (SrcPos (1, 52)),
                LC (VarDef (VarDecl "z" (TypeGeneric "c")) ExpNothing) (SrcPos (1, 62))
            ],
            symbol_pos          = SrcPos (1, 21)
        }])

    it "generics 3" $ do -- no extra spaces.
        let parsed = parse_sl "forall a b c . func doidera(x:a):void{let y:b;let z:c;}"
        parsed `shouldBe` (Right $ Program [FuncDef {
            symbol_name         = "doidera",
            function_rtype      = TypeVoid,
            function_parameters = [ VarDecl "x" (TypeGeneric "a") ],
            function_gtypes     = [ "a", "b", "c" ],
            function_body       = [
                LC (VarDef (VarDecl "y" (TypeGeneric "b")) ExpNothing) (SrcPos (1, 46)),
                LC (VarDef (VarDecl "z" (TypeGeneric "c")) ExpNothing) (SrcPos (1, 54))
            ],
            symbol_pos          = SrcPos (1, 21)
        }])


expr_specs :: Spec
expr_specs = describe "Parsing expressions" $ do
    it "precedence 1" $ do
        let parsed = parse_sl_exp "4 + 5.0 / 3"
        parsed `shouldBe` (Right $ (ExpSum  (ExpLitInteger 4) (ExpDiv (ExpLitFloating 5.0) (ExpLitInteger 3))))

    it "precedence 2" $ do
        let parsed = parse_sl_exp "4 * (4 - 3)"
        parsed `shouldBe` (Right $ (ExpMul (ExpLitInteger 4) (ExpSub (ExpLitInteger 4) (ExpLitInteger 3))))

    it "signedness" $ do
        let parsed = parse_sl_exp "- 65"
        parsed `shouldBe` (Right $ (ExpNegative $ ExpLitInteger $ 65))    


cflx_specs :: Spec
cflx_specs = describe "Parsing Control Flux" $ do
    it "only if" $ do
        let parsed = parse_sl "func main() : void { if (true) {} }"
        parsed `shouldBe` (Right $ Program [FuncDef {
            symbol_name       = "main",
            function_rtype      = TypeVoid,
            function_parameters = [],
            function_gtypes     = [],
            function_body       = [
                LC (If (ExpLitBoolean True) [] []) (SrcPos (1, 22))
            ],
            symbol_pos          = SrcPos (1, 6)
        }])

    it "if then else" $ do
        let parsed = parse_sl "func main() : void { if (true) { let x: int; } else { let y: int; } }"
        parsed `shouldBe` (Right $ Program [FuncDef {
            symbol_name       = "main",
            function_rtype      = TypeVoid,
            function_parameters = [],
            function_gtypes     = [],
            function_body       = [
                LC (If (ExpLitBoolean True) 
                    [ LC (VarDef (VarDecl "x" TypeInt) ExpNothing) (SrcPos (1, 44)) ] 
                    [ LC (VarDef (VarDecl "y" TypeInt) ExpNothing) (SrcPos (1, 65)) ]
                    ) (SrcPos (1, 22))
            ],
            symbol_pos          = SrcPos (1, 6)
        }])

    it "if elif else" $ do
        let parsed = parse_sl "func main() : void { if (a && 4) { let x: int; } elif(true) { let y: int; } else { let z: int; } }"
        parsed `shouldBe` (Right $ Program [FuncDef {
            symbol_name       = "main",
            function_rtype      = TypeVoid,
            function_parameters = [],
            function_gtypes     = [],
            function_body       = [
                LC (If (ExpAnd (ExpVariable $ VarAccess "a" VarAccessNothing) (ExpLitInteger 4))
                [ LC (VarDef (VarDecl "x" TypeInt) ExpNothing) (SrcPos (1, 46)) ] 
                [ LC (If (ExpLitBoolean True) 
                    [ LC (VarDef (VarDecl "y" TypeInt) ExpNothing) (SrcPos (1, 73))] 
                    [ LC (VarDef (VarDecl "z" TypeInt) ExpNothing) (SrcPos (1, 94))]) (SrcPos (1, 50)) ]
                ) (SrcPos (1, 22))
            ],
            symbol_pos          = SrcPos (1, 6)
        }])


array_specs :: Spec
array_specs = describe "Parsing arrays" $ do
    it "array instancing" $ do
        let parsed = parse_sl "func main() : void { let array : int[] = []; let array1 : int[3] = [1, 2 , 3]; let array2 : int[][2] = [[], []]; }"
        parsed `shouldBe` (Right $ Program [FuncDef {
            symbol_name       = "main",
            function_rtype      = TypeVoid,
            function_parameters = [],
            function_gtypes     = [],
            function_body       = [
                LC (VarDef (VarDecl "array" (TypeArray TypeInt [ExpNothing])) (ExpArrayInstancing [])) (SrcPos (1, 44)),
                LC (VarDef (VarDecl "array1" (TypeArray TypeInt [(ExpLitInteger 3)])) (ExpArrayInstancing [ExpLitInteger 1, ExpLitInteger 2, ExpLitInteger 3])) (SrcPos (1, 78)),
                LC (VarDef (VarDecl "array2" (TypeArray TypeInt [ExpNothing, (ExpLitInteger 2)])) (ExpArrayInstancing [(ExpArrayInstancing []), (ExpArrayInstancing [])])) (SrcPos (1, 112))
            ],
            symbol_pos          = SrcPos (1, 6)
        }])


-- Parsing the program from a file directly.
fparse :: FilePath -> IO (Either Error IR_Program)
fparse filepath = do
    file_content <- readFile filepath :: IO String
    return $ parse_sl file_content


read_expected_program :: FilePath -> IO IR_Program
read_expected_program filepath = do
    file_content <- readFile filepath
    return $ read $ file_content


parse_and_compare_program :: FilePath -> FilePath -> IO ()
parse_and_compare_program src src_ir = do
    parsed <- fparse src
    print $ parsed

    expected <- read_expected_program src_ir
    shouldBe parsed $ Right expected


program_specs :: Spec
program_specs = describe "Parsing programs" $ do
    it "struct then function" $ do
        let parsed = parse_sl "struct cmrd { x: int; }\nfunc camarada() : void {}"
        parsed `shouldBe` (Right $ Program [
            StructDef {
                symbol_name         = "cmrd",
                fields              = [VarDecl "x" TypeInt],
                symbol_pos          = SrcPos (1, 8)
            },

            FuncDef {
                symbol_name         = "camarada",
                function_rtype      = TypeVoid,
                function_parameters = [],
                function_gtypes     = [],
                function_body       = [],
                symbol_pos          = SrcPos (2, 6)
            }])

    it "ex1.sl" $ parse_and_compare_program "data/sl/ex1.sl" "tests/data/expected/ex1.ir"
    it "ex2.sl" $ parse_and_compare_program "data/sl/ex2.sl" "tests/data/expected/ex2.ir"
    it "ex3.sl" $ parse_and_compare_program "data/sl/ex3.sl" "tests/data/expected/ex3.ir"
    it "ex4.sl" $ parse_and_compare_program "data/sl/ex4.sl" "tests/data/expected/ex4.ir"
    it "ex5.sl" $ parse_and_compare_program "data/sl/ex5.sl" "tests/data/expected/ex5.ir"
