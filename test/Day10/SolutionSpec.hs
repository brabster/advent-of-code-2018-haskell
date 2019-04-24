{-# LANGUAGE NamedFieldPuns #-}

module Day10.SolutionSpec(spec) where

import Test.Hspec

import Day10.Solution

import qualified Data.Sequence as Seq
import qualified Data.Map as Map


spec :: Spec
spec = 
    describe "Part 1" $ do
        it "parses correctly" $ do
            str <- readFile "src/Day10/input_small.txt"
            let parsed = length $ fromString str 
            parsed `shouldReturn` 33