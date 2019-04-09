module Day8.SolutionSpec(spec) where

import Test.Hspec

import Day8.Solution


spec :: Spec
spec = do
    describe "Part 1" $ do
        it "small input is correct" $ do
            input <- readFile "src/Day8/input_small.txt"
            let parsed = readNumbers input
            sumMeta parsed `shouldBe` 138
        it "full input is correct" $ do
            input <- readFile "src/Day8/input.txt"
            let parsed = readNumbers input
            sumMeta parsed `shouldBe` 40701
    describe "Part 2" $ do
        it "small input is correct" $ do
            input <- readFile "src/Day8/input_small.txt"
            let parsed = (parseNode . readNumbers) input
            valueOf parsed `shouldBe` 66
        it "full input is correct" $ do
            input <- readFile "src/Day8/input.txt"
            let parsed = (parseNode . readNumbers) input
            valueOf parsed `shouldBe` 21399