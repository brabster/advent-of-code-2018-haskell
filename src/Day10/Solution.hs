{-# LANGUAGE NamedFieldPuns #-}

module Day10.Solution where

import Text.Regex
import qualified Data.List as List
import Data.Maybe
import Debug.Trace

data Vec = Vec { x :: Int, y :: Int } deriving (Show, Eq)
data Light = Light { position :: Vec, velocity :: Vec } deriving (Show, Eq)

lineRegex :: Regex
lineRegex = mkRegex $ partStr ++ " " ++ partStr
    where partStr = "(\\w+)=<\\s*([0-9-]+),\\s*([0-9-]+)>"

parseLine :: String -> Light
parseLine s = Light { position = pos, velocity = vel }
    where 
        Just (_:x:y:_:x1:y1:_) = matchRegex lineRegex s
        pos = Vec {x = read x :: Int, y = read y :: Int}
        vel = Vec {x = read x1 :: Int, y = read y1 :: Int}

fromString :: String -> [Light]
fromString = fmap parseLine . lines

step :: Light -> Light
step light@Light { position, velocity } = light { position = newPos }
    where newPos = Vec {
        x = (x position) + (x velocity)
        , y = (y position) + (y velocity)
    }

bounds :: [Light] -> (Vec, Vec)
bounds lights = (Vec { x = xMax, y = yMax }, Vec{ x = xMin, y = yMin })
    where
        xMax = maximum $ fmap (x . position) lights
        yMax = maximum $ fmap (y . position) lights
        xMin = minimum $ fmap (x . position) lights
        yMin = minimum $ fmap (y . position) lights
        
area :: (Vec, Vec) -> Int
area (bottomRight, topLeft) = (x bottomRight - x topLeft) * (y bottomRight - y topLeft)

findNextMinimum :: ([Light] -> Int) -> ([Light], (Maybe Int, Int)) -> ([Light], (Int, Int))
findNextMinimum f (lights, (Nothing, _)) = findNextMinimum f (lights, (Just $ f lights, 0))
findNextMinimum f (lights, (Just last, sec)) = if nextVal > last
    then (lights, (last, sec))
    else findNextMinimum f (nextLights, (Just $ nextVal, succ sec))
    where 
        nextLights = fmap step lights
        nextVal = f nextLights


printLights :: [Light] -> IO ()
printLights lights = putStrLn $ concatMap (\(v, sym) -> sym ++ if y v == y bottomLeft then "\n" else " ") annotated
    where 
        (bottomLeft, topRight) = bounds lights
        points = [Vec {x = x, y = y}  | x <- [x topRight..x bottomLeft], y <- [y topRight.. y bottomLeft]]
        annotated = fmap (\v -> (v, if isJust $ List.find ((== v) . position) lights then "#" else ".")) points