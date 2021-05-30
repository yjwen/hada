module JobGraph ( JobGraph, job, jedge, redge
                , addJob, addResult, findTodoJob, doAllJobs
                , jobTopoFoldl, jobTopoFoldr
                , resultTopoFoldl, resultTopoFoldr
                ) where

import Algebra.Graph (Graph(..), vertex, overlay, edge)
import TopoGraph (findTopoLastTrue, topoFoldr, topoFoldl)
import Data.Either(isLeft)
import Data.Function((&))

-- JobGraph traces to-do jobs and those already done and the
-- results. The node of a JobGraph is either a job or a result. An
-- edge j→r from a job j to a result r represents a done job and the
-- result it produced. A job node j which is the topo-last node of the
-- JobGraph represents a to-do job.
type JobGraph j r = Graph (Either j r) -- j the Job type, r the result type

job :: j -> JobGraph j r
job  = vertex . Left

jedge :: r -> j -> JobGraph j r
jedge r j = edge (Right r) (Left j)

redge :: j -> r -> JobGraph j r
redge j r = edge (Left j) (Right r)

addJob :: r -> j -> (JobGraph j r) -> (JobGraph j r)
addJob j j' = overlay (jedge j j')

addResult :: j -> r -> (JobGraph j r) -> (JobGraph j r)
addResult j r = overlay (redge j r)

findTodoJob :: (Eq j, Eq r) => JobGraph j r -> Maybe j
findTodoJob g = case findTopoLastTrue isLeft g of
                  Just (Left j) -> Just j
                  _ -> Nothing

-- doAllJobs f g tries to find a to-do job j in g, obtain the job
-- result and new jobs from f j and update g with the result and new
-- jobs. The process repeats until no to-do job can be found and the
-- final JobGraph is returned.
doAllJobs :: (Eq j, Eq r) => (j -> ([j], r)) -> JobGraph j r -> JobGraph j r
doAllJobs uf g =
  case findTodoJob g of
    Nothing -> g -- No to-do job. All done. 
    Just j -> doAllJobs uf g'
      where g' = foldl (&) g (map (addJob r) newjs) & addResult j r
            (newjs, r) = uf j
    

-- topoFoldl on jobs only
jobTopoFoldl :: (Eq j, Eq r) => (b -> j -> b) -> b -> JobGraph j r -> (b, JobGraph j r)
jobTopoFoldl f = topoFoldl ff
  where ff b (Left j) = f b j
        ff b (Right _) = b

-- topoFoldr on jobs only
jobTopoFoldr :: (Eq j, Eq r) => (j -> b -> b) -> b -> JobGraph j r -> (b, JobGraph j r)
jobTopoFoldr f = topoFoldr ff
  where ff (Left j) b = f j b
        ff (Right _) b = b

-- topoFoldl on results only
resultTopoFoldl :: (Eq j, Eq r) => (b -> r -> b) -> b -> JobGraph j r -> (b, JobGraph j r)
resultTopoFoldl f = topoFoldl ff
  where ff b (Left j) = b
        ff b (Right r) = f b r

-- topoFoldr on results only
resultTopoFoldr :: (Eq j, Eq r) => (r -> b -> b) -> b -> JobGraph j r -> (b, JobGraph j r)
resultTopoFoldr f = topoFoldr ff
  where ff (Left j) b = b
        ff (Right r) b = f r b
