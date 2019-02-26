module Day4.Solution where

import Text.Regex
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.List.Grouping
import Data.Ord

type GuardId = String
data Action = Start GuardId | FallsAsleep | WakesUp deriving (Show, Eq, Ord)
type Timestamp = String
data Entry = Entry Timestamp Action deriving (Show, Eq, Ord)

parseAction :: [String] -> Action
parseAction ("Guard":id:_) = Start id
parseAction ("falls":_) = FallsAsleep
parseAction ("wakes":_) = WakesUp
parseAction _ = error "Unknown action"

parseLine :: String -> Entry
parseLine line =
    let
        Just (timestamp:action:_) = matchRegex (mkRegex "\\[([-0-9 :]+)\\] (.*)") line
        parsedAction = parseAction (words action)
    in
        Entry timestamp parsedAction

assignActionsToGuards :: [Entry] -> [(GuardId, Entry)]
assignActionsToGuards entries =
    let
        step entry@(Entry _ action) assigned =
            case action of
                Start newGuardId -> (newGuardId, entry) : assigned
                _ -> (guardId, entry) : assigned
                    where (guardId, _) = head assigned
    in
        foldr step [] entries

isSleepAction :: Entry -> Bool
isSleepAction (Entry _ action) =
    case action of
        FallsAsleep -> True
        WakesUp -> True
        _ -> False

partitionListByShift :: [Entry] -> [[Entry]]
partitionListByShift = foldl (\b a ->
    case a of
        Entry _ (Start _) -> [a] : b
        _ -> case b of
            (x:xs) -> (x ++ [a]) : xs
            [] -> error "no initial start action") []

guardId :: Entry -> GuardId
guardId (Entry _ (Start id)) = id

minsFromTimestamp :: String -> Int
minsFromTimestamp ts = 
    let
        minsRegex = (mkRegex ":([0-9]+)")
        Just (minsStr:_) = matchRegex minsRegex ts
    in
        read minsStr

minutesBetween :: Timestamp -> Timestamp -> [Int]
minutesBetween start end = [(minsFromTimestamp start)..(minsFromTimestamp end - 1)]

entryListToSleepingMinutes :: [Entry] -> [[Int]]
entryListToSleepingMinutes ((Entry _ (Start id)):rest) =
    fmap (\(start:end:_) -> minutesBetween start end) $ splitEvery 2 $ fmap (\(Entry ts _) -> ts) rest

solveEx1 input =
    let
        m = Map.map concat $ Map.fromListWith (++) $ fmap (\(id, entries) -> (id, entryListToSleepingMinutes entries)) $ List.sort $ fmap (\xs -> (guardId $ head xs, xs)) $ partitionListByShift $ List.sort $ map parseLine $ lines input
        (id, mins) = List.maximumBy (comparing (length . snd)) $ Map.toList m
        (minMostAsleep:_) = List.maximumBy (comparing length) $ List.group $ List.sort mins
    in
        minMostAsleep * (read $ drop 1 id)

ex1 source = do
    input <- readFile source
    return $ solveEx1 input

frequencies :: Ord a => [a] -> Map.Map a Integer
frequencies xs = Map.fromListWith (+) $ zip xs $ repeat 1

maxOccurs :: Map.Map a Integer -> (a, Integer)
maxOccurs = List.maximumBy (comparing snd) . Map.toList

solveEx2 input =
    let
        m = Map.map concat $ Map.fromListWith (++) $ fmap (\(id, entries) -> (id, entryListToSleepingMinutes entries)) $ List.sort $ fmap (\xs -> (guardId $ head xs, xs)) $ partitionListByShift $ List.sort $ map parseLine $ lines input
        byMinuteFreq = Map.map frequencies m
        findMostAsleepMinute = Map.map maxOccurs $ Map.filter (not . Map.null) byMinuteFreq
        (id, (minute, freq)) = List.maximumBy (comparing snd) $ Map.toList findMostAsleepMinute
    in
        (read $ drop 1 id) * minute

ex2 source = do
    input <- readFile source
    return $ solveEx2 input