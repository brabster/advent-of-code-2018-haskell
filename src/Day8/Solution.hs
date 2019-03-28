{-# LANGUAGE NamedFieldPuns #-}

module Day8.Solution where

import qualified Data.List as List

data Header = Header Int Int deriving(Show)
type Meta = [Int]

data Node = Node {
    children :: [Node]
    , metadata :: [Int]
} deriving (Show, Eq)

readInt :: String -> Int
readInt = read

readNumbers :: String -> [Int]
readNumbers = (fmap readInt) . words

parseHeader :: [Int] -> (Header, [Int])
parseHeader (nodeCount:metaCount:rest) = (Header nodeCount metaCount, rest)

parseMeta :: Int -> [Int] -> (Meta, [Int])
parseMeta = List.splitAt

parseNodes :: Int -> [Node] -> [Int] -> ([Node], [Int])
parseNodes 0 nodes ints = (nodes, ints)
parseNodes count nodes ints = parseNodes (count - 1) (newNode : nodes) rest
    where (newNode, rest) = parseNode ints

extractMeta :: Node -> Meta
extractMeta node = concatMap extractMeta (children node) ++ metadata node

parseNode :: [Int] -> (Node, [Int])
parseNode ints =
    let
        (Header nodeCount metaCount, nodesAnd) = parseHeader ints
        (children, metaAnd) = parseNodes nodeCount [] nodesAnd
        (meta, rest) = parseMeta metaCount metaAnd
    in
        (Node { children = children, metadata = meta}, rest)

sumMeta :: [Int] -> Int
sumMeta = sum . extractMeta . fst . parseNode