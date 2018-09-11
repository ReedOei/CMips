module Analysis.AnalyzerTest where

import Test.Hspec

import Analysis.Analyzer
import Analysis.Patterns
import Analysis.Analyzers.ArrayLength
import Parser

-- TODO: The below currently is not yet implemented
-- analyzerTests = do
    -- describe "arrayBoundsAnalyzer" $
    --     it "searches for negative index accesses and reports them" $ do
    --         file <- loadFile "test-res/unsafe-array.c"

