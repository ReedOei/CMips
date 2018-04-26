module ParserTest where

import Test.Hspec

import Data.Either
import Data.Maybe

import Text.ParserCombinators.Parsec

import CLanguage
import Parser

parserTests = do
    describe "fileParser" $ do
        it "parses an arbitrary C file without error" $
            parse (fileParser "") "" "int f(int x) {\nreturn x;\n}" `shouldSatisfy` isRight

        it "parses a multiple functions in a C file, including prototypes" $
            let (Right (CFile name elements)) = parse (fileParser "") "" "char test(int *p);\nint f(int x) {\nreturn x + 5;\n}" in
                elements `shouldSatisfy` ((2 == ) . length)

    describe "functionParser" $
        it "parses function declarations without error" $
            parse functionParser "" "int f(int x) {\n    int n;\n\n    if (n == 0) {\n        return n + 2;\n    }\n\n    return n + 2 + 6 - 4 % x;\n}\n\n" `shouldSatisfy` isRight

    describe "ifStatementParser" $ do
        it "parses if statements without error" $
            parse ifStatementParser "" "if (x == 0) {\nreturn x + 4;\n}" `shouldSatisfy` isRight

        it "parses if statements correct" $
            let (Right res) = parse ifStatementParser "" "if (x == 0) {\nreturn x + 4;\n}" in
                res `shouldBe` IfStatement (CBinaryOp CEQ (VarRef "x") (LitInt 0)) Nothing [Return (Just (CBinaryOp Add (VarRef "x") (LitInt 4)))]

    describe "varParser" $ do
        it "parses normal variables" $ do
            let (Right res) = parse varParser "" "int x"
            res `shouldBe` Var (Type Value (NamedType "int")) "x"

        it "parses function pointers variables" $ do
            let (Right res) = parse varParser "" "int (*f)(int)"
            res `shouldBe` Var (FunctionPointer (Type Value (NamedType "int")) [Type Value (NamedType "int")]) "f"

    describe "varDefParser" $ do
        it "parses variable declarations" $ do
            let (Right res) = parse varDefParser "" "int x = 4;"
            res `shouldBe` VarDef (Var (Type Value (NamedType "int")) "x") (Just (LitInt 4))

        it "parses variable declarations with struct" $ do
            let (Right res) = parse varDefParser "" "struct Asteroid *a;"
            res `shouldBe` VarDef (Var (Type Pointer (NamedType "struct Asteroid")) "a") Nothing

        -- it "parses variable declarations with unsigned" $ do
        --     let (Right res) = parse varDefParser "" "unsigned x = 10;"
        --     res `shouldBe` VarDef (Var (Type Value (NamedType "unsigned")) "x") (Just (LitInt 10))

        -- it "parses variable declarations with long long" $ do
        --     let (Right res) = parse varDefParser "" "long long x = 10;"
        --     res `shouldBe` VarDef (Var (Type Value (NamedType "long long")) "x") (Just (LitInt 10))

        it "parses variable declarations with both unsigned and long" $ do
            let (Right res) = parse varDefParser "" "unsigned long int x = 10;"
            res `shouldBe` VarDef (Var (Type Value (NamedType "unsigned long int")) "x") (Just (LitInt 10))

    describe "block" $ do
        it "parses a block of C statements without error" $
            parse block "" "{\nint x;\nint y;\n}" `shouldSatisfy` isRight

        it "parses empty blocks correctly" $
            parse block "" "{}" `shouldBe` Right []

    describe "cArithParser" $ do
        it "parses arithmetic expressions without error" $
            parse cArithParser "" "n + 2" `shouldSatisfy` isRight

        it "parses arithmetic expressions correctly" $
            let (Right res) = parse cArithParser "" "n + 2" in
                res `shouldBe` CBinaryOp Add (VarRef "n") (LitInt 2)

        it "handles operator precedence" $
            let (Right res) = parse cArithParser "" "n + 2 + 6" in
                res `shouldBe` CBinaryOp Add (CBinaryOp Add (VarRef "n") (LitInt 2)) (LitInt 6)

