module Analysis.PatternsTest where

import Test.Hspec

import Analysis.Analyzers.ArrayLength
import Analysis.Patterns

import CLanguage
import Parser
import Types

patternsTests =
    describe "match" $
        it "searches for patterns in a program's AST and returns them" $ do

        file <- loadFile "test-res/max.c"

        let matches = allFileMatches arrayAccessPattern file :: [Context CExpression]

        length matches `shouldBe` 3

