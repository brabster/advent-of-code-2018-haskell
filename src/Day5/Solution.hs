module Day5.Solution where

import Data.Char

oppositePolarity :: Char -> Char -> Bool
oppositePolarity a b = toUpper a == toUpper b && a /= b

reactStep :: Char -> String -> String
reactStep next [] = [next]
reactStep next (last:rest) =
    if oppositePolarity next last
        then rest
        else next:last:rest

react :: String -> String
react = foldr reactStep ""

removeAllIgnoreCase :: Char -> String -> String
removeAllIgnoreCase this = filter equalsIgnoreCase
        where
            equalsIgnoreCase = (/= (toUpper this)) . toUpper


bestImprovement :: String -> Int
bestImprovement string =
    let
        unitTypeInputPairs = zip ['A'..'Z'] $ repeat string
        lengthOfModifiedReacted (c, input) = length $ react $ removeAllIgnoreCase c input
    in
        minimum $ fmap lengthOfModifiedReacted unitTypeInputPairs