module Day6.Solution where

import Debug.Trace
import Text.Regex
import qualified Data.List as List
import Data.Maybe
import qualified Data.Set as Set

data Point = Point { x :: Int, y :: Int } deriving (Show, Eq, Ord)
data BoundingBox = BoundingBox { topLeft :: Point, bottomRight :: Point } deriving (Show, Eq)

parseLine :: String -> Point
parseLine line = Point { x = read x, y = read y }
    where Just (x:y:_) = matchRegex (mkRegex "^([0-9]+), ([0-9]+)") line

{-
boundingBox :: Point -> Point -> BoundingBox
boundingBox p1 p2 = BoundingBox {
    topLeft = Point {x = xMin, y = yMin },
    bottomRight = Point { x = xMax, y = yMax } }
    where
        xMax = max (x p1) (x p2)
        yMax = max (y p1) (y p2)
        xMin = min (x p1) (x p2)
        yMin = min (y p1) (y p2)
-}

expandBoundingBox :: BoundingBox -> Point -> BoundingBox
expandBoundingBox box point =
    let
        topLeftPoint = topLeft box
        bottomRightPoint = bottomRight box
        xMax = maximum $ fmap x [point, bottomRightPoint]
        yMax = maximum $ fmap y [point, bottomRightPoint]
        xMin = minimum $ fmap x [point, topLeftPoint]
        yMin = minimum $ fmap y [point, topLeftPoint]
    in
        BoundingBox {
            topLeft = Point { x = xMin, y = xMin },
            bottomRight = Point { x = xMax, y = yMax } }

overallBoundingBox :: [Point] -> BoundingBox
overallBoundingBox (a:rest) = foldr (flip expandBoundingBox) (BoundingBox a a) rest

generateCells :: BoundingBox -> [Point]
generateCells box =
    let
        columns = [x $ topLeft box .. x $ bottomRight box]
        rows = [y $ topLeft box .. y $ bottomRight box ]
    in
        [Point { x = x, y = y } | x <- columns, y <- rows]

manhattanDistance :: Point -> Point -> Int
manhattanDistance a b = dist x + dist y
    where dist axis = abs $ foldr (-) 0 $ fmap axis [a, b]

nearestPoint :: [Point] -> Point -> Maybe Point
nearestPoint candidates point =
    let
        nearestGroup = head $ List.groupBy (\(a, _) (b, _) -> a == b) $ List.sortOn fst $ fmap (\candidate -> (manhattanDistance point candidate, candidate)) candidates
    in
        if length nearestGroup > 1
            then Nothing
            else Just $ snd $ head nearestGroup
    

nearestPoints :: BoundingBox -> [Point] -> [(Maybe Point, Point)]
nearestPoints box candidates =
    fmap (\point -> (nearestPoint candidates point, point)) (generateCells box)

isOnEdge :: BoundingBox -> Point -> Bool
isOnEdge box point = x point == x (topLeft box) || x point == x (bottomRight box) || y point == y (topLeft box) || y point == y (bottomRight box)

ex1 :: [Point] -> Int
ex1 candidates =
    let
        boundary = overallBoundingBox candidates
        nearestNeighbours = nearestPoints boundary candidates
        withInfiniteAreas = Set.fromList $ mapMaybe fst $ filter (isOnEdge boundary . snd) nearestNeighbours
        areas = fmap length $ List.group $ List.sort $ filter (flip Set.notMember withInfiniteAreas) $ mapMaybe fst nearestNeighbours
    in
        maximum areas

ex2 :: Int -> [Point] -> Int
ex2 maxDistance candidates =
    let
        boundary = BoundingBox { topLeft = Point {x=0, y = 0}, bottomRight = Point {x = 1000, y = 1000}} --overallBoundingBox candidates
        pointsInBoundary = generateCells boundary
        sumOfDistancesToCandidates p = sum $ fmap (manhattanDistance p) candidates
        withTotalDistances = fmap sumOfDistancesToCandidates pointsInBoundary
    in
        length $ filter (<= maxDistance) withTotalDistances