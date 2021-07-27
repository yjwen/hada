module JobGraph ( JobRecord(..), ExactJR(..)
                , JobGraph(..), todo, done
                , isTodo, isDone
                , jobEq, jobEqTodo, jobEqDone
                , vertexTodo, vertexDone
                , getResult
                , hasDone
                , addTodo, setDone, mustSetDone
                , findTodoJob, doAllJobs
                , jobTopoFoldl, jobTopoFoldr
                , resultTopoFoldl, resultTopoFoldr
                ) where

import Algebra.Graph (Graph(..), vertex, overlay, edge, foldg, replaceVertex)
import TopoGraph (findTopoLastTrue, topoFoldr, topoFoldl)
import Data.Maybe (isJust, isNothing)


-- A JobRecord j r 
data JobRecord j r = JobRecord { jobOf :: j
                               , resultOf :: Maybe r }
                     deriving (Ord)
todo :: j -> JobRecord j r
todo j = JobRecord j Nothing

done :: j -> r -> JobRecord j r
done j r = JobRecord j $ Just r

instance Eq j => Eq (JobRecord j r) where
  JobRecord j0 rm0 == JobRecord j1 rm1 = (j0 == j1) && (isJust rm0 == isJust rm1)
  -- Result status matters in JobRecord equity, but the result value doesn't

instance (Show j, Show r) => Show (JobRecord j r) where
  show (JobRecord j r) = show (j, r)
  

-- Wrapper for exact equalty
newtype ExactJR j r = ExactJR (JobRecord j r) deriving (Show, Ord)

instance (Eq j, Eq r) => Eq (ExactJR j r) where
  ExactJR jr0 == ExactJR jr1 = jobOf jr0 == jobOf jr1 && resultOf jr0 == resultOf jr1

-- Comparison between Job and Job record
jobEq :: Eq j => j -> JobRecord j r -> Bool
jobEq j jr = j == jobOf jr

isTodo :: JobRecord j r -> Bool
isTodo = isNothing . resultOf

isDone :: JobRecord j r -> Bool
isDone = isJust . resultOf

jobEqTodo :: Eq j => j -> JobRecord j r -> Bool
jobEqTodo j jr = isTodo jr && jobEq j jr

jobEqDone :: Eq j => j -> JobRecord j r -> Bool
jobEqDone j jr = isDone jr && jobEq j jr

type JobGraph j r = Graph (JobRecord j r)
-- JobGraph records to-do jobs, done jobs with their results and
-- relations between jobs.  The meaning of an edge j→k is
-- context-based. It might represents k depends on j being done, or k
-- is derived from j, or both

vertexTodo :: j -> JobGraph j r
vertexTodo = vertex . todo

vertexDone :: j -> r -> JobGraph j r
vertexDone j r = vertex (done j r)

-- Get result of job j in the job graph
getResult :: Eq j => j -> JobGraph j r -> Maybe r
getResult j = foldg Nothing get merge merge
  where get jr = if jobEq j jr
                 then resultOf jr
                 else Nothing
        merge l r = case l of
                      Just _ -> l
                      _ -> r
-- Check if j is done in the job graph
hasDone :: Eq j => j -> JobGraph j r -> Bool
hasDone j = isJust . getResult j

-- Add an edge from an existing job record to a to-do job to the job
-- graph ONLY when the job is not done yet
addTodo :: Eq j => JobRecord j r -> j -> JobGraph j r -> JobGraph j r
addTodo jr j g = case getResult j g of
                   Nothing -> overlay g (edge jr (todo j))
                   Just r -> overlay g (edge jr (done j r))

-- Set the job as done with a result in job graph, assuming there MUST
-- be a todo j in the graph
setDone :: Eq j => j -> r -> JobGraph j r -> JobGraph j r
setDone j r = replaceVertex (todo j) (done j r)

-- Set the job as done with a result in job graph even if such job
-- doesn't exist yet
mustSetDone :: Eq j => j -> r -> JobGraph j r -> JobGraph j r
mustSetDone j r = replaceVertex (todo j) doneJ . overlay (vertex doneJ)
  where doneJ = done j r

-- Regarding a job j is done if an edge j→k exists in the job graph,
-- a to-do jobs is equivalent to a topo-last job.
findTodoJob :: (Eq j) => JobGraph j r -> Maybe j
findTodoJob = foldg Nothing get merge merge
  where get jr = if isTodo jr
                 then Just (jobOf jr)
                 else Nothing
        merge l r = case r of
                      Just _ -> r
                      _ -> l

-- Given a worker function f that accepts a job, and return the job's
-- result and any other new found jobs, doOneJob f g tries to find a
-- to-do job j in job graph g and work on that job by f j. Return both
-- the result and the updated job graph with any new found job.
doAllJobs :: (Eq j) => (j -> ([j], r)) -> JobGraph j r -> JobGraph j r
doAllJobs f g = case findTodoJob g of
                   Nothing -> g -- No to-do job. All done. 
                   Just j -> doAllJobs f g'
                     where g' = foldl (flip $ addTodo $ done j r) (setDone j r g) newjs
                           (newjs, r) = f j
-- topoFoldl on jobs
jobTopoFoldl :: (Eq j) => (b -> j -> b) -> b -> JobGraph j r -> (b, JobGraph j r)
jobTopoFoldl f = topoFoldl f'
  where f' b jr = f b (jobOf jr)

-- topoFoldr on jobs
jobTopoFoldr :: (Eq j) => (j -> b -> b) -> b -> JobGraph j r -> (b, JobGraph j r)
jobTopoFoldr f = topoFoldr (f . jobOf)

-- topoFoldl on results
resultTopoFoldl :: (Eq j) => (b -> r -> b) -> b -> JobGraph j r -> (b, JobGraph j r)
resultTopoFoldl f = topoFoldl f'
  where f' b jr = case resultOf jr of
                    Just r -> f b r
                    Nothing -> b
-- topoFoldr on results
resultTopoFoldr :: (Eq j) => (r -> b -> b) -> b -> JobGraph j r -> (b, JobGraph j r)
resultTopoFoldr f = topoFoldr f'
  where f' jr b = case resultOf jr of
                    Just r -> f r b
                    Nothing -> b
