import Test.Hspec

import Data.Either
import Data.Maybe

import Text.ParserCombinators.Parsec

import CLanguage
import Parser

main :: IO ()
main = hspec $ do
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
                res `shouldBe` IfStatement (CBinaryOp CEQ (VarRef "x") (LitInt 0)) [Return (CBinaryOp Add (VarRef "x") (LitInt 4))]

    describe "block" $ do
        it "parses a block of C statements without error" $
            parse block "" "{\nint x;\nint y;\n}" `shouldSatisfy` isRight

        it "parses empty blocks correctly" $
            parse block "" "{}" `shouldBe` Right []

    describe "arrayAccessParser" $ do
        it "parses an array access expression without error" $
            parse arrayAccessParser "" "arr[i]" `shouldSatisfy` isRight

        it "parses an array access expression correctly" $
            parse arrayAccessParser "" "arr[i]" `shouldBe` Right (CArrayAccess "arr" (VarRef "i"))

        it "parses array access expressions with sub expressions" $
            parse arrayAccessParser "" "arr[arr2[i] + 4]" `shouldBe` Right (CArrayAccess "arr" (CBinaryOp Add (CArrayAccess "arr2" (VarRef "i")) (LitInt 4)))

    describe "cArithParser" $ do
        it "parses arithmetic expressions without error" $
            parse cArithParser "" "n + 2" `shouldSatisfy` isRight

        it "parses arithmetic expressions correctly" $
            let (Right res) = parse cArithParser "" "n + 2" in
                res `shouldBe` CBinaryOp Add (VarRef "n") (LitInt 2)

        it "handles operator precedence" $
            let (Right res) = parse cArithParser "" "n + 2 + 6" in
                res `shouldBe` CBinaryOp Add (VarRef "n") (CBinaryOp Add (LitInt 2) (LitInt 6))

    describe "opParser" $ do
        it "parses an arbitrary expression with operators" $
            parse opParser "" "n + 2 + 6 - 4 % x" `shouldSatisfy` isRight

        it "has the correct number of parsed items" $
            let (Right res) = parse opParser "" "n + 2 + 6 - 4 % x" in
                res `shouldSatisfy` ((5 == ) . length)

    describe "resolve" $ do
        it "handles operator precedence" $
            resolve (map fst cArithOps) [(LitInt 1, Just "+"), (LitInt 3, Just "*"), (LitInt 4, Nothing)] `shouldBe` Just (CBinaryOp Add (LitInt 1) (CBinaryOp Mult (LitInt 3) (LitInt 4)),Nothing)

        it "handles operator precedence when working with repeated operators" $
            resolve (map fst cArithOps) [(VarRef "n",Just "+"),(LitInt 2,Just "+"),(LitInt 6,Nothing)] `shouldBe` Just (CBinaryOp Add (VarRef "n") (CBinaryOp Add (LitInt 2) (LitInt 6)),Nothing)

