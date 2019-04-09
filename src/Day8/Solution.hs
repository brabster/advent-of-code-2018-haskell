{-# LANGUAGE NamedFieldPuns #-}

module Day8.Solution where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Debug.Trace

data Header = Header Int Int deriving(Show)
type Meta = [Int]

data Node = Node {
    children :: [Node]
    , metadata :: [Int]
} deriving (Show, Eq)

-- the usual basic parsing stuff
readInt :: String -> Int
readInt = read

readNumbers :: String -> [Int]
readNumbers = (fmap readInt) . words

-- pull out the first two numbers as a Header
parseHeader :: [Int] -> (Header, [Int])
parseHeader (nodeCount:metaCount:rest) = (Header nodeCount metaCount, rest)

-- parse the meta values (same as splitAt)
parseMeta :: Int -> [Int] -> (Meta, [Int])
parseMeta = List.splitAt

parseNodes :: Int -> [Node] -> [Int] -> ([Node], [Int])
parseNodes 0 nodes ints = (nodes, ints)
parseNodes count nodes ints = parseNodes (count - 1) (newNode : nodes) rest
    where (newNode, rest) = parseNode' ints

-- parse a sequence of input numbers into a state of (Node, rest)
parseNode' :: [Int] -> (Node, [Int])
parseNode' ints =
    let
        (Header nodeCount metaCount, nodesAnd) = parseHeader ints
        (children, metaAnd) = parseNodes nodeCount [] nodesAnd
        (meta, rest) = parseMeta metaCount metaAnd
    in
        -- must reverse children to correct order for part 2
        -- took me a while to figure out order was the problem and not an off-by-one
        (Node { children = reverse children, metadata = meta}, rest)

-- parses the input sequence in a Node
parseNode :: [Int] -> Node
parseNode = fst . parseNode'

-- get meta for this node, including all child nodes
extractMeta :: Node -> Meta
extractMeta node = concatMap extractMeta (children node) ++ metadata node

-- solution to part 1
sumMeta :: [Int] -> Int
sumMeta = sum . extractMeta . parseNode

-- probably missing a better way to get the head safely
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- no network access, make my own specialised elementAt function
-- no need to explicitly handle case where index is off the end of the list
elementAt :: [a] -> Int -> Maybe a
elementAt xs n
    | n == 0 = Nothing
    | otherwise = safeHead $ drop (n - 1) xs

-- mutually recursive with valueOf
childValue :: [Node] -> Int -> Maybe Int
childValue children meta = fmap valueOf (elementAt children meta)

-- mutually recursive with childValue
valueOf :: Node -> Int
valueOf Node { children = [], metadata } = sum metadata
valueOf Node { children, metadata } = sum $ Maybe.mapMaybe (childValue children) metadata