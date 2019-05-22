module Day11.SolutionSpec(spec) where

import Test.Hspec

import Day11.Solution
import qualified Data.Matrix as Mat

spec :: Spec
spec = 
    {--describe "powerLevel examples" $ do
        it "" $ powerLevel (3, 5) 8 `shouldBe` 4
        it "" $ powerLevel (122, 79) 57 `shouldBe` -5
        it "" $ powerLevel (217, 196) 39 `shouldBe` 0
        it "" $ powerLevel (101, 153) 71 `shouldBe` 4
    describe "submatrices" $ do
        it "" $ subMatrixWithTopLeftCorner
            (Mat.fromList 4 4 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]) (2, 2)
                `shouldBe` Mat.fromList 3 3 [6,7,8,10,11,12,14,15,16]
    describe "max power examples" $ do
        it "" $ findMaxPower3x3 18 300 300 `shouldBe` ((33, 45), 29)
        it "" $ findMaxPower3x3 42 300 300 `shouldBe` ((21, 61), 30)--}
    it "part 1" $ findMaxPower 3 7672 300 300 `shouldBe` ((22, 18), 29)