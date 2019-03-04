module Day6.SolutionSpec(spec) where

import Test.Hspec

import Day6.Solution

spec :: Spec
spec = do 
    describe "Part 1" $ do
        describe "Examples" $ do
            it "parses line" $ do
                parseLine "1, 1" `shouldBe` Point { x = 1, y = 1 }
            
            it "bounds example correctly" $ do
                input <- readFile "src/Day6/input_small.txt"
                let parsed = fmap parseLine $ lines input
                overallBoundingBox parsed `shouldBe` BoundingBox {
                    topLeft = Point { x = 1, y = 1},
                    bottomRight = Point { x = 8, y = 9 } }
            it "generates area cells" $ do
                let expected = fmap parseLine ["0, 0", "0, 1", "1, 0", "1, 1"]
                generateCells (BoundingBox { topLeft = Point {x = 0, y = 0}, bottomRight = Point {x = 1, y = 1} }) `shouldBe` expected
            it "manhatten distance" $ do
                manhattanDistance Point {x = 0, y = 2} Point {x = 2, y = 0} `shouldBe` 4
            it "nearest point" $ do
                nearestPoint [Point {x = 0, y = 0}, Point {x = 2, y = 2}] Point { x = 7, y = 1 } `shouldBe` Just Point {x = 2, y = 2}
            it "nearest point when tied" $ do
                nearestPoint [Point {x = 0, y = 0}, Point {x = 2, y = 2}] Point { x = 1, y = 1 } `shouldBe` Nothing
            --it "maps neighbours" $ do
            --    nearestPoints (BoundingBox { topLeft = Point {x = 0, y = 0}, bottomRight = Point {x = 2, y = 2} }) [Point {x = 0, y = 0}, Point {x = 2, y = 2}] `shouldBe` []
            
        describe "Challenge" $ do
            it "ex1 is correct" $ do
                input <- readFile "src/Day6/input.txt"
                let parsed = fmap parseLine $ lines input
                ex1 parsed `shouldBe` 3569
            
            it "ex2 is correct" $ do
                input <- readFile "src/Day6/input.txt"
                let parsed = fmap parseLine $ lines input
                ex2 10000 parsed `shouldBe` 48978