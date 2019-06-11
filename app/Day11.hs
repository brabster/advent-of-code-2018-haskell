module Day11 where

import Day11.Solution

main :: IO ()
main = do
    serial <- getLine
    max <- getLine
    print $ (findMaxPowerEff (read serial) (read max))
