import Test.Hspec

import Data.Either
import Data.Maybe

import Text.ParserCombinators.Parsec

import CLanguage
import Parser

import CompilerTest
import OptimizerTest
import ParserTest
import SimulatorTest

main :: IO ()
main = hspec $ do
    parserTests
    simulatorTests
    optimizerTests
    compilerTests

