module SimulatorTest where

import Test.Hspec

import MIPSParser
import Simulator

simulatorTests = do
    describe "test.s" $
        it "simple test of simulator, using print and function calls" $ do
            (output, state) <- execute <$> loadMIPSFile "test-res/mips/test.s"
            output `shouldBe` ["12"]

    describe "prime.s" $
        it "lisp program that finds the list of primes under 100" $ do
            (output, state) <- execute <$> loadMIPSFile "test-res/mips/prime.s"
            output `shouldBe` ["[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]"]

