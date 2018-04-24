module CompilerTest where

import Test.Hspec

import Compiler.Compiler
import LispParser
import LispCompiler
import Parser
import Simulator

compilerTests = do
    describe "func.c" $
        it "uses function pointers" $ do
            (output, state) <- execute <$> (compile =<< loadFile "test-res/func.c")
            output `shouldBe` ["200"]

    describe "malloc.c" $
        it "uses malloc and function pointers to compute the sum of the squares of 0..9" $ do
            (output, state) <- execute <$> (compile =<< loadFile "test-res/malloc.c")
            output `shouldBe` ["285"]

    describe "optimize.c" $
        it "has several functions to test optimizations and common optimization bugs" $ do
            (output, state) <- execute <$> (compile =<< loadFile "test-res/optimize.c")
            output `shouldBe` ["3720"]

    describe "recursive.c" $
        it "has several recursive functions (e.g. fib)" $ do
            (output, state) <- execute <$> (compile =<< loadFile "test-res/recursive.c")
            output `shouldBe` ["222"]

    describe "resolve-constant.c" $
        it "uses many local variables, but all are constants, and should be resolved at compile time" $ do
            (output, state) <- execute <$> (compile =<< loadFile "test-res/resolve-constants.c")
            output `shouldBe` ["12376"]

    describe "example.lisp" $
        it "lisp program that finds the list of primes under 100" $ do
            (output, state) <- execute <$> (compile =<< compileLisp <$> loadLispFile "test-res/example.lisp")
            output `shouldBe` ["[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]"]

