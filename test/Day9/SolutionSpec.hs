{-# LANGUAGE NamedFieldPuns #-}

module Day9.SolutionSpec(spec) where

import Test.Hspec

import Day9.Solution

import qualified Data.Sequence as Seq
import qualified Data.Map as Map


spec :: Spec
spec = 
    describe "Part 1" $ do
        it "board works correctly" $ do
            rotate (mkBoard [0] 0) Clockwise 0 `shouldBe` mkBoard [0] 0
            rotate (mkBoard [0] 0) Clockwise 1 `shouldBe` mkBoard [0] 0
            rotate (mkBoard [0] 0) CounterClockwise 1 `shouldBe` mkBoard [0] 0
            rotate (mkBoard [0, 1] 0) Clockwise 1 `shouldBe` mkBoard [0, 1] 1
            rotate (mkBoard [0, 1] 1) Clockwise 1 `shouldBe` mkBoard [0, 1] 0
            rotate (mkBoard [0, 1] 0) Clockwise 2 `shouldBe` mkBoard [0, 1] 0
            rotate (mkBoard [0, 1] 1) CounterClockwise 1 `shouldBe` mkBoard [0, 1] 0
            rotate (mkBoard [0, 1] 1) CounterClockwise 2 `shouldBe` mkBoard [0, 1] 1
            rotate (mkBoard [0, 1, 2] 1) CounterClockwise 1 `shouldBe` mkBoard [0, 1, 2] 0
            rotate (mkBoard [0, 1, 2] 1) CounterClockwise 2 `shouldBe` mkBoard [0, 1, 2] 2
            rotate (mkBoard [0, 1, 2] 1) CounterClockwise 3 `shouldBe` mkBoard [0, 1, 2] 1
            rotate (mkBoard [0, 1, 2] 1) CounterClockwise 4 `shouldBe` mkBoard [0, 1, 2] 0
            rotate (mkBoard [0, 1, 2] 1) CounterClockwise 5 `shouldBe` mkBoard [0, 1, 2] 2
            rotate (mkBoard [0, 1, 2] 1) CounterClockwise 6 `shouldBe` mkBoard [0, 1, 2] 1
            rotate (mkBoard [0, 1, 2] 1) Clockwise 1 `shouldBe` mkBoard [0, 1, 2] 2
            rotate (mkBoard [0, 1, 2] 1) Clockwise 2 `shouldBe` mkBoard [0, 1, 2] 0
            rotate (mkBoard [0, 1, 2] 1) Clockwise 3 `shouldBe` mkBoard [0, 1, 2] 1
            rotate (mkBoard [0, 1, 2] 1) Clockwise 4 `shouldBe` mkBoard [0, 1, 2] 2
            rotate (mkBoard [0, 1, 2] 1) Clockwise 5 `shouldBe` mkBoard [0, 1, 2] 0
            rotate (mkBoard [0, 1, 2] 1) Clockwise 6 `shouldBe` mkBoard [0, 1, 2] 1
        it "applies a move correctly" $
            applyMove GameState { board = mkBoard [0, 1] 1, scores = Map.empty } (2, 2)
                `shouldBe` GameState { board = mkBoard [0, 2, 1] 1, scores = Map.empty }
        describe "plays sample correctly" $ do
            it "move 22 pre-score correct" $ play Game { players = 9, marbles = 22 } `shouldBe`
                parseExample "0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15" Map.empty
            it "move 23 (first scoring) correct" $ play Game { players = 9, marbles = 23 } `shouldBe`
                parseExample "0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15" (Map.fromList [(5,32)])
            it "25th marble correct" $ play Game { players = 9, marbles = 25 } `shouldBe`
                parseExample "0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15" (Map.fromList [(5,32)])
        describe "plays samples correctly" $ do
            it "ex-" $ (hiscore . play) Game { players = 1, marbles = 48 } `shouldBe` 95
            it "ex-" $ (hiscore . play) Game { players = 9, marbles = 48 } `shouldBe` 63
            it "ex1" $ (hiscore . play) Game { players = 10, marbles = 1618 } `shouldBe` 8317
            it "ex2" $ (hiscore . play) Game { players = 13, marbles = 7999 } `shouldBe` 146373
            it "ex3" $ (hiscore . play) Game { players = 17, marbles = 1104 } `shouldBe` 2764
            it "ex4" $ (hiscore . play) Game { players = 21, marbles = 6111 } `shouldBe` 54718
            it "ex5" $ (hiscore . play) Game { players = 30, marbles = 5807 } `shouldBe` 37305
        describe "challenges" $ do
            it "gets first challenge right" $ (hiscore . play) Game { players = 418, marbles = 71339 } `shouldBe` 412127
            -- slow it "gets second challenge right" $ (hiscore . play) Game { players = 418, marbles = 71339 * 100 } `shouldBe` 412127