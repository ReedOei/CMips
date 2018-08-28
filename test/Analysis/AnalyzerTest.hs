module Analysis.AnalyzerTest where

import Test.Hspec

import Analysis.Analyzer
import Analysis.Patterns
import Parser

analyzerTests =
    describe "arrayBoundsAnalyzer" $
        it "searches for negative index accesses and reports them" $ do
            file <- loadFile "test-res/unsafe-array.c"

            length (analyzeArrayBounds file) `shouldBe` 1

