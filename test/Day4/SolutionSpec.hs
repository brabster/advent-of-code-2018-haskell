module Day4.SolutionSpec(spec) where

import Test.Hspec

import Day4.Solution
import qualified Data.Map.Strict as Map

withSampleInput f = do
    input <- readFile "src/Day4/input_small.txt"
    let parsed = fmap parseLine $ lines input
    return $ f parsed

withFullInput f = do
    input <- readFile "src/Day4/input.txt"
    let parsed = fmap parseLine $ lines input
    return $ f parsed
    

spec :: Spec
spec = do
    describe "Parser" $
        it "parses good line" $
            parseLine "[1518-11-01 00:00] Guard #10 begins shift" `shouldBe` Entry "1518-11-01 00:00" (Start "#10")
        
    describe "Sequence Parsing" $ do
        it "parses an init start event" $ parseEntry (Nothing, Map.empty) (Entry "" (Start "x")) `shouldBe` (Just "x", Map.empty)
        --it "errors an init non start event" $ parseEntry (Nothing, Map.empty) (Entry "" FallsAsleep) `shouldThrow`
        it "parses a valid non-start event" $ parseEntry (Just "x", Map.empty) (Entry "" FallsAsleep) `shouldBe` (Just "x", Map.singleton "x" [(Entry "" FallsAsleep)])
        it "parses a valid non-init start event" $ parseEntry (Just "x", Map.empty) (Entry "" (Start "y")) `shouldBe` (Just "y", Map.empty)
    
    describe "entryListToMinutes" $
        it "computes correct minutes from out of order list" $
            entryListToSleepingMinutes [
                (Entry "2019-01-01 00:04" WakesUp),
                (Entry "2019-01-01 00:00" FallsAsleep),
                (Entry "2019-01-01 00:59" WakesUp),
                (Entry "2019-01-01 00:55" FallsAsleep)] `shouldBe` [[0,1,2,3], [55,56,57,58]]
    
    describe "ex1" $ do
        it "is correct for small example" $
            withSampleInput getGuardMostMinsAsleep `shouldReturn` 240
        it "is correct for problem" $
            withFullInput getGuardMostMinsAsleep `shouldReturn` 84636
                
                
    describe "ex2" $ do
        it "is correct for small example" $
            withSampleInput getGuardAndMinMostAsleep `shouldReturn` 4455
        it "is correct for full example" $
            withFullInput getGuardAndMinMostAsleep `shouldReturn` 91679
        
   