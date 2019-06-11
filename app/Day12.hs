module Day12 where

import Day12.Solution

main :: IO ()
main = do
    iterations <- getLine
    inputFile <- getLine
    input <- readFile inputFile
    print $ iterateAndSum (read iterations) input
