{-  -------------------------------
    @file       tests/LexerTests.hs
    @details    testing the lexer...
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module LexerTests (tests) where

import Test.Hspec
import Frontend.Error
import Frontend.Lexer
import Frontend.Token


tokenizef_plain :: FilePath -> IO [Lexeme]
tokenizef_plain filepath = do
    file_content <- readFile filepath :: IO String

    -- lexer applied to the file content...
    return $ tokenize_plain file_content


tokenizef :: FilePath -> IO (Either Error [Token])
tokenizef filepath = do
    file_content <- readFile filepath :: IO String
    return $ tokenize file_content


le :: String -> (Int, Int) -> Either Error a
le s p = Left $ Error LexicalError s (SrcPos p)



tests :: Spec
tests = describe "Lexer tests" $ do
    -- tests appear to be organized in a tree-like structure; that's handy...
    error_specs
    numerical_specs
    io_specs
    expr_specs

error_specs :: Spec
error_specs = describe "Errors" $ do
    it "unclosed commentary" $ 
        tokenize "/* absolutamente nada" `shouldBe` (le "Unclosed comment." (1, 22))

    it "unexpected close commentary" $ 
        tokenize "func ok(void) : void {} */" `shouldBe` (le "Unexpected close comment." (1, 25))

    it "unknown symbol 1" $ 
        tokenize "ol#" `shouldBe` (le "Unknown symbol: \"#\"." (1, 3))

    it "unknown symbol 2" $ 
        tokenize "olá" `shouldBe` (le "Unknown symbol: \"á\"." (1, 3))


numerical_specs :: Spec
numerical_specs = describe "Numerical data" $ do
    it "tokenizing integral 1" $
        tokenize "10" `shouldBe` Right [Token (SrcPos (1, 1)) (T_Integral 10), Token (SrcPos (1, 3)) T_EOF]
    
    it "tokenizing integral 2" $
        tokenize "257" `shouldBe` Right [Token (SrcPos (1, 1)) (T_Integral 257), Token (SrcPos (1, 4)) T_EOF]

    it "tokenizing integral 3" $
        tokenize "+ 100178" `shouldBe` Right [Token (SrcPos (1, 1)) T_Plus, Token (SrcPos (1, 3)) (T_Integral 100178), Token (SrcPos (1, 9)) T_EOF]

    it "tokenizing integral 4" $ -- EOF at 11
        tokenize_plain "9876543210" `shouldBe` [T_Integral 9876543210, T_EOF]

    it "tokenizing float (π)" $
        tokenize "3.141592653589793" `shouldBe` Right [Token (SrcPos (1, 1)) (T_Floating 3.141592653589793), Token (SrcPos (1, 18)) T_EOF]

    it "tokenizing float (ϕ)" $
        tokenize "1.618033988749895" `shouldBe` Right [Token (SrcPos (1, 1)) (T_Floating 1.618033988749895), Token (SrcPos (1, 18)) T_EOF]

    {- 
    it "tokenizing float (.5)" $
        tokenize ".578" `shouldBe` Right [Token (1, 1) (T_Floating 0.578), Token (1, 5) T_EOF]
    -}

    it "tokenizing float scientific notation 1" $
        tokenize "1.57e9" `shouldBe` Right [Token (SrcPos (1, 1)) (T_Floating (1.57e9 :: Double)), Token (SrcPos (1, 7)) T_EOF]

    it "tokenizing float scientific notation 2" $
        tokenize "1.57e-9" `shouldBe` Right [Token (SrcPos (1, 1)) (T_Floating (1.57e-9 :: Double)), Token (SrcPos (1, 8)) T_EOF]

    it "tokenizing float scientific notation 3" $
        tokenize "1.5755E+9" `shouldBe` Right [Token (SrcPos (1, 1)) (T_Floating (1.5755E+9 :: Double)), Token (SrcPos (1, 10)) T_EOF]


-- From what follows, it is pretty daunting to have the tokens listed expliclty here,
-- so it is rather stored externally.
read_expected_tokens :: FilePath -> IO [Token]
read_expected_tokens filepath = do
    file_content <- readFile filepath
    return $ map read $ lines file_content

-- futurely, this may be further subdivided...
io_specs :: Spec
io_specs = describe "IO data" $ do
    it "ex1.sl" $ do
        tokens <- tokenizef "data/sl/ex1.sl"
        expected <- read_expected_tokens "tests/data/expected/ex1.tokens"
        shouldBe tokens $ Right expected 

    it "ex2.sl" $ do
        tokens <- tokenizef "data/sl/ex2.sl"
        expected <- read_expected_tokens "tests/data/expected/ex2.tokens"
        shouldBe tokens $ Right expected 

    it "ex3.sl" $ do
        tokens <- tokenizef "data/sl/ex3.sl"
        expected <- read_expected_tokens "tests/data/expected/ex3.tokens"
        shouldBe tokens $ Right expected 

    it "ex4.sl" $ do
        tokens <- tokenizef "data/sl/ex4.sl"
        expected <- read_expected_tokens "tests/data/expected/ex4.tokens"
        shouldBe tokens $ Right expected 

    it "ex5.sl" $ do
        tokens <- tokenizef "data/sl/ex5.sl"
        expected <- read_expected_tokens "tests/data/expected/ex5.tokens"
        shouldBe tokens $ Right expected 


expr_specs :: Spec -- just for ...
expr_specs = describe "Expressions" $ do
    it "expr boolean literal" $ do
        tokenize_plain "true && false" `shouldBe` [T_Bool True, T_ExpAnd, T_Bool False, T_EOF]

    it "expr example 1" $ do
        tokenize_plain "4 + 5.0 / 3" `shouldBe` [T_Integral 4, T_Plus, T_Floating 5.0, T_Divide, T_Integral 3, T_EOF]

    it "expr example 2" $ do
        tokenize_plain "4 * (5 - 4)" `shouldBe` [T_Integral 4, T_Times, T_LParenthesis, T_Integral 5, T_Minus, T_Integral 4, T_RParenthesis, T_EOF]

    it "expr negative" $ do
        tokenize_plain "4 * -4" `shouldBe` [T_Integral 4, T_Times, T_Minus, T_Integral 4, T_EOF]
