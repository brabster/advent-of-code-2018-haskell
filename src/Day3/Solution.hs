module Day3.Solution(ex1, ex2) where

import Text.Regex
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Point = Point Int Int deriving (Eq, Show, Ord)
data Dimensions = Dimensions Int Int deriving (Show)
data Claim = Claim String Point Dimensions deriving (Show)

linePattern = mkRegex "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"

parseLine :: String -> Claim
parseLine line =
    let
        Just (id:cxt:cyt:wt:ht:_) = matchRegex linePattern line
    in
        Claim id (Point (read cxt) (read cyt)) (Dimensions (read wt) (read ht))

claimAreaPoints :: Claim -> [Point]
claimAreaPoints (Claim id (Point x1 y1) (Dimensions w h)) =
    map (uncurry Point) [(x, y) | x <- [x1..x1 + w - 1], y <- [y1..y1 + h - 1]]

appendClaimArea :: Claim -> Map.Map Point [Claim] -> Map.Map Point [Claim]
appendClaimArea claim reg =
    let
        claimPoints = zip (claimAreaPoints claim) (repeat claim)
        step (point, claim) = Map.insertWith (++) point [claim]
    in
        foldr step reg claimPoints

applyClaims :: [Claim] -> Map.Map Point [Claim]
applyClaims = foldr appendClaimArea Map.empty

step :: (Set.Set String, Set.Set String) -> [Claim] -> (Set.Set String, Set.Set String)
step (seen, overlapping) claims =
    let
        claimIds = map (\(Claim id _ _) -> id) claims
        newSeen = foldr Set.insert seen claimIds
    in
        (newSeen, if length claims > 1 
            then foldr Set.insert overlapping claimIds 
            else overlapping)

findNonOverlappingClaims :: Map.Map Point [Claim] -> Set.Set String
findNonOverlappingClaims mapping =
    let
        claimLists = Map.elems mapping
        (seen, overlapping) = foldl step (Set.empty, Set.empty) claimLists
    in
        Set.difference seen overlapping

ex1 = do
    input <- readFile "src/Day3/input.txt"
    let claims = map parseLine $ lines input
    return $ Map.size $ Map.filter (> 1) $ Map.map length $ applyClaims claims

ex2 = do
    input <- readFile "src/Day3/input.txt"
    let claims = map parseLine $ lines input
    return $ findNonOverlappingClaims $ applyClaims claims
