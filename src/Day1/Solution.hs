module Day1.Solution(ex1, ex2) where

import qualified Data.Set as Set

readIntUnsafe :: String -> Integer
readIntUnsafe (sign:rest) =
    let
        safeSign = [sign | sign == '-']
        input = safeSign ++ rest
    in read input :: Integer

parseInput :: String -> [Integer]
parseInput = fmap readIntUnsafe . lines

solve1 :: String -> Integer
solve1 = sum . parseInput 

ex1 = do
        input <- readFile "src/Day1/input.txt"
        return $ solve1 input

firstRepeatingInternal :: Integer -> Set.Set Integer -> [Integer] -> Integer
firstRepeatingInternal curr seen (x:xs) = 
    let next = x + curr in
    if Set.member next seen 
        then next
        else firstRepeatingInternal next (Set.insert next seen) xs

firstRepeating :: [Integer] -> Integer
firstRepeating = firstRepeatingInternal 0 Set.empty

ex2 = do
        input <- readFile "src/Day1/input.txt"
        return $ firstRepeating $ cycle $ parseInput input
