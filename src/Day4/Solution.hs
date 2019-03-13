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

type ParseState = (Maybe GuardId, Map.Map GuardId [Entry])

parseEntry :: ParseState -> Entry -> ParseState
parseEntry (Nothing, m) (Entry _ (Start guard)) = (Just guard, m)
parseEntry (Nothing, m) otherEntry = error "Need a start event when no current guard ID"
parseEntry (Just guard, m) (Entry _ (Start nextGuard)) = (Just nextGuard, m)
parseEntry (Just guard, m) entry = (Just guard, (Map.insertWith (++) guard [entry]) m)

guardId :: Entry -> GuardId
guardId (Entry _ (Start id)) = id

numericGuardId :: GuardId -> Int
numericGuardId = read . (drop 1)

timestamp :: Entry -> Timestamp
timestamp (Entry ts _) = ts

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
entryListToSleepingMinutes entries =
    fmap (\mins -> case mins of
        [] -> []
        [start] -> error start
        (start:end:_) -> minutesBetween start end) $ splitEvery 2 $ List.sort $ fmap timestamp entries

frequencies :: Ord a => [a] -> Map.Map a Integer
frequencies xs = Map.fromListWith (+) $ zip xs $ repeat 1

maxOccurs :: Map.Map a Integer -> (a, Integer)
maxOccurs = List.maximumBy (comparing snd) . Map.toList

entriesByGuard :: [Entry] -> Map.Map GuardId [Entry]
entriesByGuard = snd . foldl parseEntry (Nothing, Map.empty) . List.sort

getGuardMostMinsAsleep :: [Entry] -> Int
getGuardMostMinsAsleep entries =
    let
        sleepingMinutesByGuard = Map.map (concat . entryListToSleepingMinutes) $ entriesByGuard entries
        (mostMinutesAsleep, mins) = List.maximumBy (comparing (length . snd)) $ Map.toList sleepingMinutesByGuard
        (min, _) = List.maximumBy (comparing snd) $ Map.toList $ frequencies mins
    in
        numericGuardId mostMinutesAsleep * min

getGuardAndMinMostAsleep :: [Entry] -> Int
getGuardAndMinMostAsleep entries =
    let
        maxSleepingMinutesByGuard = Map.map maxOccurs $ Map.map (frequencies . concat . entryListToSleepingMinutes) $ entriesByGuard entries
        (id, (minute, _)) = List.maximumBy (comparing (snd . snd)) $ Map.toList maxSleepingMinutesByGuard
    in
        numericGuardId id * minute
