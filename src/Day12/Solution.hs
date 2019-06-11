{-# LANGUAGE NamedFieldPuns #-}

module Day12.Solution (iterateAndSum) where

import Text.ParserCombinators.ReadP
import Data.List
import Debug.Trace

isPositionFlag :: Char -> Bool
isPositionFlag char = char `elem` "#."

positionFlag :: ReadP Char
positionFlag = satisfy isPositionFlag

type Pot = (Int, Char)
type PlantConfig = [Pot]
type Fragment = String
data Rule = Rule { fragment :: Fragment, result :: Char } deriving (Show)
data PlantState = PlantState { config :: PlantConfig, rules :: [Rule]} deriving (Show)

rule :: ReadP Rule
rule = do
    skipSpaces
    fragment <- many1 positionFlag
    skipSpaces
    string "=>"
    skipSpaces
    result <- positionFlag
    skipSpaces
    return (Rule fragment result)
    
plantState :: ReadP PlantState
plantState = do
    string "initial state: "
    initConfig <- many1 positionFlag
    skipSpaces
    rules <- many1 rule
    skipSpaces
    return (PlantState (zip [0..length initConfig] initConfig) rules)

toSublistsOf5 :: [a] -> [[a]]
toSublistsOf5 xs = fmap (flip slice xs) [0 .. length xs - 5]
    where slice n = (take 5).(drop n)

applyRules :: [Rule] -> [Pot] -> Pot
applyRules rules candidate =
    case find ((== (fmap snd candidate)) . fragment) rules of
        Nothing -> (i, '.')
        Just Rule { result } -> (i, result)
    where
        i = (fst . head . drop 2) candidate

parseInput :: String -> PlantState
parseInput s = fst <$> last $ readP_to_S plantState s

expandConfig :: [Pot] -> [Pot]
expandConfig pots =
    [(i, '.') | i <- [lowest - 5..lowest - 1]]
    ++ pots
    ++ [(i, '.') | i <- [highest + 1..highest + 5]]
        where
            lowest = (fst.head) pots
            highest = (fst.last) pots

applyGeneration :: PlantState -> PlantState
applyGeneration PlantState { config, rules } = PlantState {
    rules,
    config = fmap (applyRules rules) (toSublistsOf5 (expandConfig config))
}

applyGenerations :: Int -> PlantState -> PlantState
applyGenerations n init = foldl step init [1..n]
        where
            step :: PlantState -> Int -> PlantState
            step acc x = applyGeneration acc

{-
5B is too many - look for a pattern with this
-390,-35,290,-7,96,134,-62,314,-443,363,13,-94,64,136,-272,189,44,89,-197,291,-132,60,-141,119,74,96,-130,124,67,-129,179,-24,98,-91,172,-93,155,-153,208,-95,187,-247,360,-285,239,-231,256,-183,304,-295,388,-179,273,-203,328,-186,212,-73,168,-100,229,-89,116,21,75,-9,142,-4,32,64,33,-6,103,-10,80,16,82,-57,155,-64,138,-44,143,-120,219,-130,208,-116,216,-126,160,-70,157,-61,101,-5,48,48,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51

answer is (51*(50000000000-98))+6193
-}
applyGenerationsScan :: Int -> PlantState -> [PlantState]
applyGenerationsScan n init = scanl step init [1..n]
        where
            step :: PlantState -> Int -> PlantState
            step acc x = applyGeneration acc

sumPopulatedPots :: [Pot] -> Int
sumPopulatedPots = sum . fmap fst <$> filter ((== '#').snd)

iterateAndSum :: Int -> String -> Int
iterateAndSum n s
    = sumPopulatedPots $ config $ applyGenerations n $ parseInput s