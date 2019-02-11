module Day2.Solution(ex1, ex2) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

frequencies :: Ord a => [a] -> Map.Map a Integer
frequencies xs = Map.fromListWith (+) $ zip xs $ repeat 1

hasFrequencyOf :: Integer -> Map.Map a Integer -> Bool
hasFrequencyOf n = any ((== n) . snd) . Map.toList

solve1 :: [String] -> Int
solve1 boxIDs =
    let
        freqs = map frequencies boxIDs
        has2 = length $ filter (hasFrequencyOf 2) freqs
        has3 = length $ filter (hasFrequencyOf 3) freqs
    in
        has2 * has3

ex1 = do
        input <- readFile "src/Day2/input.txt"
        return $ solve1 $ lines input

pairElementsEqual :: Eq a => a -> a -> Bool
pairElementsEqual a b = a == b

editDistance :: String -> String -> Int
editDistance s1 s2 = abs (length s1) - length s2 + length (filter not $ zipWith pairElementsEqual s1 s2)

solve2 boxIDs =
    let
        combinations = [(x, y) | x <- boxIDs, y <- boxIDs, x /= y ]
        (((s1, s2), _):_) = filter ((== 1) . snd) $ zip combinations (map (uncurry editDistance) combinations)
    in
        map fst $ filter (uncurry pairElementsEqual) $ zip s1 s2

ex2 = do
        input <- readFile "src/Day2/input.txt"
        let result = solve2 $ lines input
        return result

--check = ex2 >>= \result -> result == "bvnfawcnyoeyudzrpgslimtkj"