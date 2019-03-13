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
        
    describe "Challenge" $ do
        it "small input is correct" $ do
            input <- readFile "src/Day7/input_small.txt"
            let parsed = input2Dependencies input
            findJobOrder parsed `shouldBe` "CABDFE"
        it "ex1 is correct" $ do
            input <- readFile "src/Day7/input.txt"
            let parsed = input2Dependencies input
            findJobOrder parsed `shouldBe` "LFMNJRTQVZCHIABKPXYEUGWDSO"