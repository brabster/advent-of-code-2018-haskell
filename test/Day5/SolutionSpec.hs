module Day5.SolutionSpec(spec) where

import Test.Hspec

import Day5.Solution

spec :: Spec
spec = do 
    describe "Part 1" $ do
        describe "Examples" $ do
            it "reacts" $ do
                react "aA" `shouldBe` ""
            it "reacts" $ do
                react "abBA" `shouldBe` ""
            it "reacts" $ do
                react "abAB" `shouldBe` "abAB"
            it "reacts" $ do
                react "aabAAB" `shouldBe` "aabAAB"
            it "reacts" $ do
                react "dabAcCaCBAcCcaDA" `shouldBe` "dabCBAcaDA"
            it "reacts" $ do
                react "abBA" `shouldBe` ""
        
        describe "Challenge" $ do
            it "is correct" $ do
                input <- readFile "src/Day5/input.txt"
                let reactedLength = length $ react input
                reactedLength `shouldBe` 9348

    describe "Part 2" $ do
        describe "Examples" $ do
            it "has miminum possible reacted length" $ do
                bestImprovement "dabAcCaCBAcCcaDA" `shouldBe` 4
        
        describe "Challenge" $ do
            it "is correct" $ do
                input <- readFile "src/Day5/input.txt"
                let minReactedLength = bestImprovement input
                minReactedLength `shouldBe` 4996

    {-
    ,("abBA", ""),("abAB", "abAB"),("aabAAB", "aabAAB"),("dabAcCaCBAcCcaDA", "dabCBAcaDA")]
    describe "ex1" $ do
        it "is correct for small example" $ do
            ex1 "src/Day4/input_small.txt" `shouldReturn` 240
        it "is correct for problem" $ do
            ex1 "src/Day4/input.txt" `shouldReturn` 84636
        
    describe "ex1" $ do
        it "is correct for small example" $ do
            ex2 "src/Day4/input_small.txt" `shouldReturn` 4455
        it "is correct for small example" $ do
            ex2 "src/Day4/input.txt" `shouldReturn` 91679
    -}