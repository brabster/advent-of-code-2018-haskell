module Day7.SolutionSpec(spec) where

import Test.Hspec

import Day7.Solution

sampleDepGraph = [("A",["B","D"]),("B",["E"]),("C",["A","F"]),("D",["E"]),("F",["E"])]

spec :: Spec
spec = do
    describe "Part 1" $ do
        describe "Parsing" $
            it "parses line" $
                parseLine "Step F must be finished before step E can begin." `shouldBe` ("F", "E")
        describe "dependency processing" $ do
            it "handles empty list" $ buildDepGraph [] `shouldBe` []
            it "handles single element" $ buildDepGraph [("A", "B")] `shouldBe` [("A", ["B"])]
            it "handles two elements same prereq" $
                buildDepGraph [("A", "C"), ("A", "B")] `shouldBe` [("A", ["B", "C"])]
        describe "job times" $ do
            it "looks up value correctly" $ jobTime 5 "G" `shouldBe` Just 12
            it "looks up missing value correctly" $ jobTime 5 "GA" `shouldBe` Nothing
        
    describe "Challenge" $ do
        it "small input is correct" $ do
            input <- readFile "src/Day7/input_small.txt"
            let parsed = input2Dependencies input
            findJobOrder parsed `shouldBe` "CABDFE"
        it "ex1 is correct" $ do
            input <- readFile "src/Day7/input.txt"
            let parsed = input2Dependencies input
            findJobOrder parsed `shouldBe` "LFMNJRTQVZCHIABKPXYEUGWDSO"
        it "small input is correct" $ do
            input <- readFile "src/Day7/input_small.txt"
            let parsed = input2Dependencies input
            getBuildTime 2 0 parsed `shouldBe` 15
        it "ex2 is correct" $ do
            input <- readFile "src/Day7/input.txt"
            let parsed = input2Dependencies input
            getBuildTime 5 60 parsed `shouldBe` 1180