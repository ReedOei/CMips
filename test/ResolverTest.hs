module ResolverTest where

import Control.Lens
import Control.Monad.State

import Test.Hspec

import CLanguage
import Compiler.Resolver
import Compiler.Types
import Types

asteroidStruct = StructDef "Asteroid" [Var [] (Type Value (NamedType "int")) "yx",Var [] (Type Value (NamedType "int")) "points"]
asteroidMapStruct = StructDef "AsteroidMap" [Var [] (Type Value (NamedType "unsigned int")) "length",Var [] (Array 50 (Type Value (NamedType "Asteroid"))) "asteroids"]
canvasStruct = StructDef "Canvas" [Var [] (Type Value (NamedType "unsigned int")) "height",Var [] (Type Value (NamedType "unsigned int")) "width",Var [] (Type Value (NamedType "unsigned char")) "pattern",Var [] (Type Pointer (Type Pointer (NamedType "char"))) "canvas"]

testEnv = set file (CFile "test.c" [asteroidStruct, asteroidMapStruct]) emptyEnvironment

resolverTests =
    describe "getStructOffset" $ do
        it "computes the offset for a basic struct at the beginning" $ do
            let (offset, _) = runState (structOffset asteroidMapStruct "length") testEnv

            offset `shouldBe` 0

        it "computes the offset for a basic struct at the end" $ do
            let (offset, _) = runState (structOffset asteroidStruct "points") testEnv

            offset `shouldBe` 4

        it "accounts for padding in the struct when calculating the size" $
            sizeof (StructType canvasStruct) `shouldBe` 16

