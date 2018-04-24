module OptimizerTest where

import Control.Lens
import Control.Monad.State

import Test.Hspec

import Compiler.Types
import Optimizer
import MIPSLanguage

-- faster path expected = do
--     (output, state) <- execute <$> (compileWith (set optimizeLevel 1 defaultCompile) =<< loadFile path)
--     output `shouldBe` expected
--     view executed state

optimizerTests =
    describe "optimizeMovedResults" $ do
        it "removes unnecessary moves by putting results directly into final destination" $ do
            let (instrs, _) = runState (optimizeMovedResults [Inst OP_ADD "t1" "t2" "t6", Inst OP_MOVE "t0" "t1" ""]) emptyEnvironment
            instrs `shouldBe` [Inst OP_ADD "t0" "t2" "t6"]

        it "uses transitivity of move operation" $ do
            let (instrs, _) = runState (optimizeMovedResults [Inst OP_MOVE "t1" "t2" "", Inst OP_MOVE "t0" "t1" ""]) emptyEnvironment
            instrs `shouldBe` [Inst OP_MOVE "t0" "t2" ""]

    -- describe "optimize" $ do
    --     it "uses function pointers" $ do
    --         (output, state) <- execute <$> (compile =<< loadFile "test-res/func.c")
    --         output `shouldBe` ["200"]

    --     it "uses malloc and function pointers to compute the sum of the squares of 0..9" $ do
    --         (output, state) <- execute <$> (compile =<< loadFile "test-res/malloc.c")
    --         output `shouldBe` ["285"]

    --     it "has several functions to test optimizations and common optimization bugs" $ do
    --         (output, state) <- execute <$> (compile =<< loadFile "test-res/optimize.c")
    --         output `shouldBe` ["3720"]

    --     it "has several recursive functions (e.g. fib)" $ do
    --         (output, state) <- execute <$> (compile =<< loadFile "test-res/recursive.c")
    --         output `shouldBe` ["222"]

    -- describe "resolve-constant.c" $
    --     it "uses many local variables, but all are constants, and should be resolved at compile time" $ do
    --         (output, state) <- execute <$> (compile =<< loadFile "test-res/resolve-constants.c")
    --         output `shouldBe` ["12376"]

    -- describe "example.lisp" $
    --     it "lisp program that finds the list of primes under 100" $ do
    --         (output, state) <- execute <$> (compile =<< compileLisp <$> loadLispFile "test-res/example.lisp")
    --         output `shouldBe` ["[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]"]

    --     it "doesn't make the program slower for optimize.c" $ do
    --         let (_, state) = runState

    --     it "doesn't make the program slower for optimize.c" $ do
    --         let (_, state) = runState

    --     it "doesn't make the program slower for optimize.c" $ do
    --         let (_, state) = runState

    --     it "doesn't make the program slower for optimize.c" $ do
    --         let (_, state) = runState

    --     it "produces the same results for optimize.c"

    --     it "produces the same results for recursive.c"

    --     it "produces the same results for example.lisp"

