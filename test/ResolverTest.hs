module ResolverTest where

import Control.Lens
import Control.Monad.State

import Test.Hspec

import CLanguage
import Compiler.Resolver
import Compiler.Types

asteroidStruct = StructDef "Asteroid" [Var (Type Value (NamedType "int")) "yx",Var (Type Value (NamedType "int")) "points"]
asteroidMapStruct = StructDef "AsteroidMap" [Var (Type Value (NamedType "unsigned int")) "length",Var (Array 50 (Type Value (NamedType "Asteroid"))) "asteroids"]

testEnv = set file (CFile "test.c" [asteroidStruct, asteroidMapStruct]) emptyEnvironment

resolverTests =
    describe "getStructOffset" $ do
        it "computes the offset for a basic struct at the beginning" $ do
            let (offset, _) = runState (structOffset asteroidMapStruct "length") testEnv

            offset `shouldBe` 0

        it "computes the offset for a basic struct at the end" $ do
            let (offset, _) = runState (structOffset asteroidStruct "points") testEnv

            offset `shouldBe` 4

