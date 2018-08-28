import Test.Hspec

import Data.Either
import Data.Maybe

import Text.ParserCombinators.Parsec

import CLanguage
import Parser

import Analysis.AnalyzerTest
import Analysis.PatternsTest
import CompilerTest
import OptimizerTest
import ParserTest
import ResolverTest
import SimulatorTest

main :: IO ()
main = hspec $ do
    parserTests
    simulatorTests
    optimizerTests
    resolverTests
    compilerTests
    analyzerTests
    patternsTests

