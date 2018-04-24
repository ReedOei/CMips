module CompilerTest where

import Test.Hspec

import Compiler.Compiler
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

