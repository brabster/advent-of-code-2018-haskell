module Day4.SolutionSpec(spec) where

import Test.Hspec

import Day4.Solution

spec :: Spec
spec = do 
    describe "Parser" $ do
        it "parses good line" $ do
            parseLine "[1518-11-01 00:00] Guard #10 begins shift" `shouldBe` Entry "1518-11-01 00:00" (Start "#10")
    
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