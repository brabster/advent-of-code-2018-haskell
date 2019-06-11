module Day11.Solution (findMaxPowerEff) where

import qualified Data.Matrix as Mat
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Ord
import Data.Char
import Debug.Trace
import Data.MemoCombinators as Memo

powerLevel :: Int -> (Int, Int) -> Int
powerLevel serial (x, y) =
    let
        rackId = x + 10
        b = rackId * y
        c = b + serial
        d = c * rackId
        hundredsDigit = (!!2) . reverse . show
        e = digitToInt (hundredsDigit d)
    in
        e - 5

powerLevels :: Int -> Int -> Int -> Mat.Matrix Int
powerLevels serial rows cols = Mat.matrix rows cols (powerLevel serial)
    
subMatrixWithTopLeftCorner :: Int -> Mat.Matrix Int -> (Int, Int) -> Mat.Matrix Int
subMatrixWithTopLeftCorner dim m (i, j) = Mat.submatrix i (i+dim-1) j (j+dim-1) m

getEdgeSum :: Mat.Matrix Int -> Int -> (Int, Int) -> Int
getEdgeSum matrix dim (i, j) = (sum rightmost) + sum (take (dim - 1) lowest)
    where
        edge (i, j) (i', j') = Mat.toList $ Mat.submatrix i i' j j' matrix
        rightmost = edge (i + dim - 1, j) (i + dim - 1, j + dim - 1)
        lowest = edge (i, j + dim - 1) (i + dim - 1, j + dim - 1)

getPower ((_, _), p, _) = p

findMaxPowerStep' :: Mat.Matrix Int -> Int -> (Int, Int) -> ((Int, Int), Int, Int)
findMaxPowerStep' = findMaxPowerStep

findMaxPowerStep :: Mat.Matrix Int -> Int -> (Int, Int) -> ((Int, Int), Int, Int)
findMaxPowerStep matrix dim (i, j)
    | dim == 1 = ((i, j), Mat.getElem i j matrix, dim)
    | otherwise = if getPower candidate > getPower previous then candidate else previous
        where
            previous = findMaxPowerStep' matrix (dim - 1) (i, j)
            candidate = ((i, j), (getPower previous) + getEdgeSum matrix dim (i, j), dim)

findMaxPowerForDim :: Mat.Matrix Int -> Int -> Int -> ((Int, Int), Int, Int)
findMaxPowerForDim matrix max dim = List.maximumBy (comparing getPower) squares
    where
        topLefts = [(x, y) | x <- [1..max-dim], y <- [1..max-dim]]
        squares = fmap (findMaxPowerStep matrix dim) topLefts

findMaxPowerEff serial max = List.maximumBy (comparing getPower) dims
    where dims = fmap (findMaxPowerForDim (powerLevels serial max max) max) [1..max-1]

-- too slow...
findMaxPower :: Int -> Int -> Int -> ((Int, Int), Int)
findMaxPower dim serial max = maxPower
    where
        initial = powerLevels serial max max
        topCorners = [(x, y) | x <- [1..max-dim], y <- [1..max-dim]]
        subMatrices = fmap (subMatrixWithTopLeftCorner (traceShowId dim) initial) topCorners
        indexedSums = zip topCorners (fmap sum subMatrices)
        maxPower = List.maximumBy (comparing snd) indexedSums

-- too slow
--findMaxPowerAllSizes :: Int -> Int -> ((Int, Int), Int, Int)
findMaxPowerAllSizes serial max = List.maximumBy (\((_, a),_) ((_, b), _) -> compare a b) maxes
    where maxes = [ (findMaxPower size serial max, size) | size <- [1..max-1]]