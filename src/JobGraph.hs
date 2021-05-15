module JobGraph (JobGraph, job, jedge, redge,
                 addJob, addResult, findTodoJob, doAllJobs) where

import Algebra.Graph (Graph(..), vertex, overlay, edge)
import TopoGraph (findTopoLastTrue)
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

jedge :: j -> j -> JobGraph j r
jedge j j' = edge (Left j) (Left j')

redge :: j -> r -> JobGraph j r
redge j r = edge (Left j) (Right r)

addJob :: j -> j -> (JobGraph j r) -> (JobGraph j r)
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
      where g' = foldl (&) g (map (addJob j) newjs) & addResult j r
            (newjs, r) = uf j
    

