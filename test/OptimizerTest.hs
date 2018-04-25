module OptimizerTest where

import Control.Lens
import Control.Monad.State

import Test.Hspec

import Compiler.Types
import Compiler.Compiler
import LispParser
import LispCompiler
import Parser
import Simulator
import Optimizer
import MIPSLanguage

faster program expected = do
    (outputNoOpt, stateNoOpt) <- execute <$> (compileWith (set optimizeLevel 0 defaultCompileOptions) =<< program)
    outputNoOpt `shouldBe` expected

    (outputOpt, stateOpt) <- execute <$> (compileWith (set optimizeLevel 1 defaultCompileOptions) =<< program)
    outputOpt `shouldBe` expected

    let noOptTime = view executed stateNoOpt
    let optTime = view executed stateOpt

    (optTime < noOptTime) `shouldBe` True

    liftIO $ putStrLn $ "Program takes " ++ show (noOptTime - optTime) ++ " fewer instructions (" ++ show (fromIntegral optTime / fromIntegral noOptTime * 100) ++ "%)"

fasterC path = faster (loadFile path)

optimizerTests = do
    describe "optimizeMovedResults" $ do
        it "removes unnecessary moves by putting results directly into final destination" $ do
            let (instrs, _) = runState (optimizeMovedResults [Inst OP_ADD "t1" "t2" "t6", Inst OP_MOVE "t0" "t1" ""]) emptyEnvironment
            instrs `shouldBe` [Inst OP_ADD "t0" "t2" "t6"]

        it "uses transitivity of move operation" $ do
            let (instrs, _) = runState (optimizeMovedResults [Inst OP_MOVE "t1" "t2" "", Inst OP_MOVE "t0" "t1" ""]) emptyEnvironment
            instrs `shouldBe` [Inst OP_MOVE "t0" "t2" ""]

        it "does not change all operands" $ do
            let (instrs, _) = runState (optimizeMovedResults [Inst OP_LW "s3" "0" "s3", Inst OP_MOVE "s4" "s3" ""]) emptyEnvironment
            instrs `shouldBe` [Inst OP_LW "s4" "0" "s3"]

    describe "optimize" $ do
        it "improves the speed but doesn't change the result of func.c" $
            fasterC "test-res/func.c" ["200"]
        it "improves the speed but doesn't change the result of malloc.c" $
            fasterC "test-res/malloc.c" ["285"]
        it "improves the speed but doesn't change the result of optimize.c" $
            fasterC "test-res/optimize.c" ["3720"]
        it "improves the speed but doesn't change the result of recursive.c" $
            fasterC "test-res/recursive.c" ["222"]
        it "improves the speed but doesn't change the result of resolve-constants.c" $
            fasterC "test-res/resolve-constants.c" ["12376"]
        it "improves the speed but doesn't change the result of example.lisp" $
            faster (compileLisp <$> loadLispFile "test-res/example.lisp") ["[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]"]
        it "improves the speed but doesn't change the result of floats.c" $
            fasterC "test-res/floats.c" ["2589.8306", "5402", "285.0"]

