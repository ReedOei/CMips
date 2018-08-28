module CompilerTest where

import Data.List (isInfixOf)

import Test.Hspec

import Compiler.Compiler
import LispParser
import LispCompiler
import MIPSLanguage
import Parser
import Simulator

compileExecute loaded = do
    Right loaded <- compile loaded
    pure $ execute loaded

compilerTests = do
    describe "func.c" $
        it "uses function pointers" $ do
            (output, state) <- compileExecute =<< loadFile "test-res/func.c"
            output `shouldBe` ["200"]

    describe "malloc.c" $
        it "uses malloc and function pointers to compute the sum of the squares of 0..9" $ do
            (output, state) <- compileExecute =<< loadFile "test-res/malloc.c"
            output `shouldBe` ["285"]

    describe "optimize.c" $
        it "has several functions to test optimizations and common optimization bugs" $ do
            (output, state) <- compileExecute =<< loadFile "test-res/optimize.c"
            output `shouldBe` ["3720"]

    describe "recursive.c" $
        it "has several recursive functions (e.g. fib)" $ do
            (output, state) <- compileExecute =<< loadFile "test-res/recursive.c"
            output `shouldBe` ["222"]

    describe "resolve-constant.c" $
        it "uses many local variables, but all are constants, and should be resolved at compile time" $ do
            (output, state) <- compileExecute =<< loadFile "test-res/resolve-constants.c"
            output `shouldBe` ["12376"]

    describe "example.lisp" $
        it "lisp program that finds the list of primes under 100" $ do
            (output, state) <- compileExecute =<< (compileLisp <$> loadLispFile "test-res/example.lisp")
            output `shouldBe` ["[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]"]

    describe "floats.c" $
        it "program using floating point numbers for math, conditions, and in arrays" $ do
            (output, state) <- compileExecute =<< loadFile "test-res/floats.c"
            output `shouldBe` ["2589.8306", "5402", "45.0", "5.0"]

    describe "inline.c" $
        it "has several function calls that should be inlined, and one that should not (it would use the stack too much and increase time)" $ do
            (output, state) <- compileExecute =<< loadFile "test-res/inline.c"
            output `shouldBe` ["100 4 175"]

