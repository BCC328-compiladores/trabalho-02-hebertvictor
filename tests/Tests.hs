{-  --------------------------
    @file       tests/Tests.hs
    @details    test suit driver...
-}

module Main where

import Test.Hspec

-- getting the other tests...
import qualified LexerTests
import qualified ParserTests
import qualified PrettyTests
import qualified SemanticTests
import qualified InterpreterTests


main :: IO ()
main = hspec $ do
    --LexerTests.tests
    --ParserTests.tests
    --PrettyTests.tests
    --SemanticTests.tests
    InterpreterTests.tests
