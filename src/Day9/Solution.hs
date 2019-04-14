{-# LANGUAGE NamedFieldPuns #-}

module Day9.Solution where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Maybe
import Debug.Trace

type MarbleId = Int
type PlayerId = Int
type Score = Int
type Board = (Seq.Seq MarbleId, Int)
data Direction = Clockwise | CounterClockwise deriving (Eq, Show)
data Game = Game { players :: Int, marbles :: Int } deriving (Eq, Show)

parseLine :: String -> Game
parseLine str = Game { players = read players :: Int, marbles = read marbles :: Int }
    where (players:_:_:_:_:_:marbles:_) = words str

data GameState = GameState {
    nextPlayer :: PlayerId
    , nextMarble :: MarbleId
    , currentMarbleIndex :: Int
    , board :: Board
    , scores :: Map.Map PlayerId Score 
    , game :: Game
} deriving (Show)

initGameState :: Game -> GameState
initGameState game = GameState {
    nextPlayer = 0
    , nextMarble = 1
    , currentMarbleIndex = 0
    , board = Seq.singleton 0
    , scores = Map.empty
    , game = game
}

getMarbleId :: Board -> Int -> Direction -> Int -> MarbleId
getMarbleId board marbleIndex dir count = (length board + marbleIndex `op` count) `mod` length board
    where op = if dir == Clockwise then (+) else (-)

placeMarble :: Board -> Int -> MarbleId -> (Board, Int, Score)
placeMarble board currentMarbleIndex nextMarble
    | nextMarble `mod` 23 == 0 =
        let
            splitLocation = getMarbleId board currentMarbleIndex CounterClockwise 7
            (before, after) = Seq.splitAt splitLocation board
        in
            (before Seq.>< Seq.drop 1 after, splitLocation, nextMarble + Seq.index after 0)
    | otherwise =
        let
            insertLocation = 1 + getMarbleId board currentMarbleIndex Clockwise 1
        in
            (Seq.insertAt insertLocation nextMarble board, insertLocation, 0)

play' :: GameState -> GameState
play' state@GameState {nextPlayer, nextMarble, currentMarbleIndex, board, scores, game = Game {marbles, players} }
    | nextMarble == marbles = state
    | otherwise =
        let (board', marbleIndex, score) = placeMarble board currentMarbleIndex nextMarble
        in play' state {
            nextPlayer = (nextPlayer + 1) `mod` players
            , nextMarble = nextMarble + 1
            , currentMarbleIndex = marbleIndex
            , board = board'
            , scores = Map.insertWith (+) nextPlayer score scores
        }

play :: Game -> GameState
play = play' . initGameState

highScore :: GameState -> Score
highScore GameState { scores } = (snd . Map.findMax) scores