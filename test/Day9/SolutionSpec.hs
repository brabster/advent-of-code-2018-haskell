{-# LANGUAGE NamedFieldPuns #-}

module Day9.SolutionSpec(spec) where

import Test.Hspec

import Day9.Solution

import qualified Data.Sequence as Seq

playToStep :: Game -> Int -> (Board, MarbleId, Score)
playToStep Game { players, marbles } steps = foldl step (Seq.singleton 0, 0, 0) [1..steps]
    where
        step :: (Board, MarbleId, Score) -> MarbleId -> (Board, MarbleId, Score)
        step (board, current, score) = placeMarble board current

sampleGame = Game {players = 9, marbles = 25}

readSampleState :: String -> Seq.Seq Int
readSampleState = Seq.fromList . fmap read . words

spec :: Spec
spec = 
    describe "Part 1" $ do
        it "parses input correctly" $ do
            input <- readFile "src/Day9/input_small.txt"
            let parsed = parseLine input
            parsed `shouldBe` sampleGame
        it "places marbles correctly" $ do
            placeMarble (Seq.singleton 0) 0 1 `shouldBe` (Seq.fromList [0, 1], 1, 0)
            playToStep sampleGame 1 `shouldBe` (Seq.fromList [0, 1], 1, 0)
            playToStep sampleGame 2 `shouldBe` (Seq.fromList [0, 2, 1], 1, 0)
            playToStep sampleGame 3 `shouldBe` (Seq.fromList [0, 2, 1, 3], 3, 0)
            playToStep sampleGame 12 `shouldBe` (readSampleState "0  8  4  9  2 10  5 11 1 12 6  3  7", 9, 0)
            playToStep sampleGame 23 `shouldBe` (readSampleState "0 16  8 17  4 18 19 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15", 6, 32)
        it "gets examples right" $ do
            highScore (play $ parseLine "10 players; last marble is worth 1618 points: high score is 8317") `shouldBe` 8317
            highScore (play $ parseLine "13 players; last marble is worth 7999 points: high score is 146373") `shouldBe` 146373
            highScore (play $ parseLine "17 players; last marble is worth 1104 points: high score is 2764") `shouldBe` 2764
            highScore (play $ parseLine "21 players; last marble is worth 6111 points: high score is 54718") `shouldBe` 54718
            highScore (play $ parseLine "30 players; last marble is worth 5807 points: high score is 37305") `shouldBe` 37305
        it "gets part 1 right" $ highScore (play $ parseLine "418 players; last marble is worth 71339 points") `shouldBe` 37305