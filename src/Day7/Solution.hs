module Day7.Solution where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as List

type JobId = String
type Dep = (JobId, JobId)
type DepGraph = [(JobId, [JobId])]

-- basic input parsing stuff
parseLine :: String -> Dep
parseLine str = (from, to)
    where (_:from:_:_:_:_:_:to:_) = words str

-- basic input parsing stuff
input2Dependencies :: String -> [Dep]
input2Dependencies = fmap parseLine . lines

--Turns a list of (from, to) dependencies
--into a list of (from, [to_1, to_2, to_3, ...])
--ensuring ascending order in both the froms and the to-lists
buildDepGraph :: [Dep] -> DepGraph
buildDepGraph = Map.toList . foldr (\(a, b) m -> Map.insertWith (++) a [b] m) Map.empty . List.sort

{-
    Inspects a dependency graph to find the next available job
    Returns a version with the next job appended to a list and the graph without the 'from' that's just been used.

    The difference between the set of all the jobs with prereqs and the set of prereqs is the set of jobs that can run now

    Tail-recursive
-}
findJobOrderStep :: ([JobId], DepGraph) -> ([JobId], DepGraph)
findJobOrderStep (jobs, graph)
    | length graph == 1 = (jobs ++ [fst (head graph)] ++ snd (head graph), []) -- catch the last step and append the last 'to' job
    | otherwise =
        let
            toJobs = Set.fromList $ concatMap snd graph

            fromJobs = Set.fromList $ fmap fst graph

            (nextJob:_) = Set.toList $ Set.difference fromJobs toJobs

            nextGraph = filter ((/= nextJob) . fst) graph
        in
            findJobOrderStep (jobs ++ [nextJob], nextGraph)

--Provides a default value for the recursive findJobOrderStep function and concats the result into a string
findJobOrder :: [Dep] -> String
findJobOrder deps = concat jobs
        where
            (jobs, _) = findJobOrderStep ([], graph)
            graph = buildDepGraph deps