{-# LANGUAGE NamedFieldPuns #-}

module Day9.Solution where

import Text.Regex
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Sequence as Seq
import Data.Maybe
import Debug.Trace

type PlayerId = Int
type MarbleValue = Int
type Score = Int
data Board = Board { circle :: Seq.Seq MarbleValue, cursor :: Int } deriving (Eq, Show)
data Game = Game { players :: Int, marbles :: Int } deriving (Eq, Show)
data GameState = GameState { board :: Board, scores :: Map.Map PlayerId Score } deriving (Eq, Show)
data Direction = Clockwise | CounterClockwise deriving (Eq, Show)

mkBoard :: [MarbleValue] -> Int -> Board
mkBoard circle cursor
    | cursor >= length circle = error "Index out of high bound"
    | cursor < 0 = error "Index out of low bound"
    | otherwise = Board (Seq.fromList circle) cursor

initBoard :: Board
initBoard = mkBoard [0] 0

initGameState :: GameState
initGameState = GameState { board = initBoard, scores = Map.empty }

directionToOp :: Direction -> (Int -> Int -> Int)
directionToOp dir = if dir == Clockwise then (+) else (-)

rotate :: Board -> Direction -> Int -> Board
rotate board@Board{ circle, cursor } direction places
    | places < 0 = error "negative rotate places"
    | otherwise = board { cursor = newCursor }
        where
            op = directionToOp direction
            rawCursor = op cursor places
            newCursor = rawCursor `mod` length circle

placeMarble :: Board -> MarbleValue -> Board
placeMarble Board { circle, cursor } val = Board { circle = Seq.insertAt newCursor val circle, cursor = newCursor }
    where newCursor = succ cursor

removeAtCursor :: Board -> (Board, MarbleValue)
removeAtCursor board@Board { circle, cursor } = (board { circle = newCircle }, removed)
    where 
        (prefix, cursorAndRest) = Seq.splitAt cursor circle
        removed = Seq.index cursorAndRest 0
        rest = Seq.deleteAt 0 cursorAndRest
        newCircle = prefix Seq.>< rest

takeMarble :: Board -> (Board, MarbleValue)
takeMarble board = removeAtCursor $ rotate board CounterClockwise 7

moves :: Game -> [(PlayerId, MarbleValue)]
moves Game { players, marbles } = zip (cycle [1..players]) [1..marbles]

applyMove :: GameState -> (PlayerId, MarbleValue) -> GameState
applyMove state@GameState { board, scores } (player, value)
    | value `mod` 23 == 0 = let (newBoard, extraScore) = takeMarble board
        in GameState { board = newBoard, scores = Map.insertWith (+) player (value + extraScore) scores }
    | otherwise = state { board = placeMarble (rotate board Clockwise 1) value }

play :: Game -> GameState
play game = List.foldl' applyMove initGameState (moves game)

hiscore :: GameState -> Int
hiscore GameState { scores } = (maximum . fmap snd . Map.toList) scores

--play :: Game -> GameState
scan game = scanl applyMove initGameState (moves game)

parseExample :: String -> Map.Map Int Int -> GameState
parseExample str scores = GameState { board = mkBoard circle cursor, scores = scores }
    where
        regex = mkRegex "^([0-9 ]*)\\(([0-9]+)\\)([0-9 ]*)$"
        (pre:cursorStr:rest) = fromMaybe [] (matchRegex regex str)
        preVals = fmap read $ words pre
        restVals = fmap read $ words $ head rest
        cursorVal = read cursorStr
        cursor = length preVals
        circle = preVals ++ [cursorVal] ++ restVals
