module Analysis.Analyzer where

import Control.Lens ((^.))

import Analysis.Analyzers.ArrayLength
import Analysis.Patterns
import Analysis.Resolution
import Analysis.Types
import Analysis.Warnings

import CLanguage
import Compiler.Context
import Compiler.Types
import Parser

doAnalysis :: FilePath -> IO [Warning]
doAnalysis path = do
    f <- loadFile path
    pure $ analyze f

analyze :: CFile -> [Warning]
analyze file = analyzeArrayBounds file

