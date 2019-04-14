{-# LANGUAGE NamedFieldPuns #-}

module Day7.Solution where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

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

-- Turns a list of (from, to) dependencies
-- into a list of (from, [to_1, to_2, to_3, ...])
-- ensuring ascending order in both the froms and the to-lists
buildDepGraph :: [Dep] -> DepGraph
buildDepGraph = Map.toList . foldr (\(a, b) m -> Map.insertWith (++) a [b] m) Map.empty . List.sort

-- Find the set of job IDs that could be started in this graph
availableJobs :: DepGraph -> [JobId]
availableJobs graph = Set.toList $ Set.difference fromJobs toJobs
    where
        toJobs = Set.fromList $ concatMap snd graph
        fromJobs = Set.fromList $ fmap fst graph

dependents :: JobId -> DepGraph -> [JobId]
dependents jobId g = maybe [] snd $ List.find ((== jobId) . fst) g

removeJobs :: [JobId] -> DepGraph -> DepGraph
removeJobs jobs = filter (notInJobs . fst)
        where notInJobs = flip Set.notMember $ Set.fromList jobs

{-
    Inspects a dependency graph to find the next available job
    Returns a version with the next job appended to a list and the graph without the 'from' that's just been used.

    The difference between the set of all the jobs with prereqs and the set of prereqs is the set of jobs that can run now

    Tail-recursive
-}
findJobOrderStep :: ([JobId], DepGraph) -> ([JobId], DepGraph)
findJobOrderStep (jobs, [(from, to)]) = (jobs ++ [from] ++ to, []) -- catch the last step and append the last 'to' job
findJobOrderStep (jobs, graph) =
        let
            (nextJob:_) = availableJobs graph

            nextGraph = removeJobs [nextJob] graph
        in
            findJobOrderStep (jobs ++ [nextJob], nextGraph)

--Provides an initial value for the recursive findJobOrderStep function and concats the result into a string
findJobOrder :: [Dep] -> String
findJobOrder deps = concat jobs
        where
            (jobs, _) = findJobOrderStep ([], graph)
            graph = buildDepGraph deps

{-
sec|active  | graph                                                                 | deps  | next
-  | -   -  | [("A",["B","D"]),("B",["E"]),("C",["A","F"]),("D",["E"]),("F",["E"])] | ABDEF | C
1  | C3  -  | [("A",["B","D"]),("B",["E"]),("D",["E"]),("F",["E"])]                 | BDE   | AF
2  | C2  -  | [("A",["B","D"]),("B",["E"]),("D",["E"]),("F",["E"])]                 | BDE   | AF
3  | C1  -  | [("A",["B","D"]),("B",["E"]),("D",["E"]),("F",["E"])]                 | BDE   | AF
4  | A1  F6 | [("B",["E"]),("D",["E"])]                                             | E     | BD
5  | B2  F5 | [("D",["E"])]                                                         | E     | D
6  | B1  F4 | [("D",["E"])]                                                         | E     | D
7  | D4  F3 | []                                                                    |       | E
8  | D3  F2 | []                                                                    |       | E
9  | D2  F1 | []                                                                    |       | E
10 | D1  -  | []                                                                    |       |
11 | -   E5 | []                                                                    |       |
12 | -   E4 | []                                                                    |       |
13 | -   E3 | []                                                                    |       |
14 | -   E2 | []                                                                    |       |
15 | -   E1 | []                                                                    |       |
-}

data BuildState = BuildState {
    second :: Int
    , activeWorkers :: [(JobId, Int)]
    , idleWorkerCount :: Int
    , graph :: DepGraph
    , jobStartTime :: Int
} deriving (Eq, Show)

-- constant list of (jobId, jobTimeSeconds)
jobTimes :: Map.Map JobId Int
jobTimes = Map.fromList $ zip (fmap (:[]) ['A'..'Z']) [1..]

-- get the time to execute the given job
jobTime :: Int -> JobId -> Maybe Int
jobTime startTime jobId = fmap (+ startTime) $ Map.lookup jobId jobTimes

-- refer to this frequently in the code that follows
type WorkerState = (JobId, Int)

-- could use a lens to edit the internal structure
subtractSecond :: WorkerState -> WorkerState
subtractSecond (jobId, sec) = (jobId, sec - 1)

-- true when this WorkerState is for a worker that's not finished
isBusy :: WorkerState -> Bool
isBusy (_, sec) = sec > 0

-- create a WorkerState for this worker with this job time left
assignWorker :: Int -> JobId -> Maybe WorkerState
assignWorker jobStartTime jobId = fmap (\t -> (jobId, t)) (jobTime jobStartTime jobId)

-- way too complicated!
parallelBuildStep :: BuildState -> BuildState
parallelBuildStep state@BuildState {activeWorkers = [], graph = []} = state
parallelBuildStep state@BuildState {second, activeWorkers, idleWorkerCount, graph, jobStartTime} =
    let
        nextStateTemplate = state { second = second + 1 }
        updatedWorkers = fmap subtractSecond activeWorkers
        (unavailableWorkers, newlyAvailableWorkers) = List.partition isBusy updatedWorkers
        availableWorkerCount = idleWorkerCount + length newlyAvailableWorkers
        nextState = case availableWorkerCount of
            0 -> nextStateTemplate { activeWorkers = updatedWorkers }
            ready ->
                let
                    justCompletedJobs = fmap fst newlyAvailableWorkers
                    newGraph = removeJobs justCompletedJobs graph
                    currentAvailablePreJobs = Set.toList $ Set.difference (Set.fromList (availableJobs newGraph)) (Set.fromList (fmap fst unavailableWorkers))
                    currentAvailableJobs = currentAvailablePreJobs ++ if newGraph == [] && graph /= [] then snd $ head graph else []
                    newlyAssignedJobs = take ready currentAvailableJobs
                    newlyAssignedWorkers = Maybe.mapMaybe (assignWorker jobStartTime) newlyAssignedJobs
                    stillIdle = availableWorkerCount - length newlyAssignedWorkers
                in
                    nextStateTemplate {
                        activeWorkers = newlyAssignedWorkers ++ unavailableWorkers
                        , idleWorkerCount = stillIdle
                        , graph = newGraph
                    }
    in parallelBuildStep nextState

getBuildTime :: Int -> Int -> [Dep] -> Int
getBuildTime workerCount jobStartTime deps = second $ parallelBuildStep BuildState {second = -1, activeWorkers = [], idleWorkerCount = workerCount, graph = buildDepGraph deps, jobStartTime}
