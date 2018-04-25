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

faster program = do
    (outputNoOpt, stateNoOpt) <- execute <$> (compileWith (set optimizeLevel 0 defaultCompileOptions) =<< program)

    (outputOpt, stateOpt) <- execute <$> (compileWith (set optimizeLevel 1 defaultCompileOptions) =<< program)

    outputNoOpt `shouldBe` outputOpt

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
            fasterC "test-res/func.c"
        it "improves the speed but doesn't change the result of malloc.c" $
            fasterC "test-res/malloc.c"
        it "improves the speed but doesn't change the result of optimize.c" $
            fasterC "test-res/optimize.c"
        it "improves the speed but doesn't change the result of recursive.c" $
            fasterC "test-res/recursive.c"
        it "improves the speed but doesn't change the result of resolve-constants.c" $
            fasterC "test-res/resolve-constants.c"
        it "improves the speed but doesn't change the result of example.lisp" $
            faster (compileLisp <$> loadLispFile "test-res/example.lisp")
        it "improves the speed but doesn't change the result of floats.c" $
            fasterC "test-res/floats.c"

