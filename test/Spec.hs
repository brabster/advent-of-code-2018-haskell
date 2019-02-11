
import Test.Hspec

import Data.Set as Set

import Day1.Solution as Day1
import Day2.Solution as Day2
import Day3.Solution as Day3

main :: IO ()
main = hspec $ do 
    describe "Day1" $ do
        it "ex1 correct" $ Day1.ex1 `shouldReturn` 425
        it "ex2 correct" $ Day1.ex2 `shouldReturn` 57538
    
    describe "Day2" $ do
        it "ex1 correct" $ Day2.ex1 `shouldReturn` 6696
        it "ex2 correct" $ Day2.ex2 `shouldReturn` "bvnfawcnyoeyudzrpgslimtkj"

    describe "Day3" $ do
        it "ex1 correct" $ Day3.ex1 `shouldReturn` 106501
        it "ex2 correct" $ Day3.ex2 `shouldReturn` Set.singleton "632"