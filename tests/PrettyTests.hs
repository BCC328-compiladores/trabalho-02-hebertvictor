{-  -------------------------------
    @file       tests/PrettyTests.hs
    @details    ...
-}

module PrettyTests (tests) where

import Test.Hspec
import Frontend.Token ( SrcPos(..) )
import Frontend.Pretty
import Frontend.IR


tests :: Spec
tests = describe "Pretty tests" $ do
    pure_specs
    general_specs -- to split


pure_specs :: Spec
pure_specs = describe "pure" $ do
    it "type" $ do
        pretty_sl TypeVoid `shouldBe` "void"

    it "void var" $ do
        pretty_sl (VarDef (VarDecl "x" TypeVoid) ExpNothing) `shouldBe` "let x;"


general_specs :: Spec
general_specs = describe "general" $ do
    it "parameters" $ do
        let pstr = pretty_sl $ FuncDef "camarada" TypeVoid [VarDecl "x" TypeInt, VarDecl "y" TypeVoid] [] [] (SrcPos (0, 0))
        pstr `shouldBe` "func camarada(x : int, y) : void {\n\t\n}"
