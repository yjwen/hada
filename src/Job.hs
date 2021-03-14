module Job(doAllJobs, doAllJobs') where

-- j: The job type
-- r: The result type
-- 
-- Given a function that accepts a job and returns a result and
-- possible new jobs, starts from a initial job list and result value,
-- return the final result when there is no new job to do. Job type j
-- must be an instance of Eq so that doAllJobs can avoid doing
-- duplicated jobs. Result type r must be an instance of Semigroup so
-- that two results can be merged into one.
doAllJobs :: (Eq j, Semigroup r) => (j -> ([j], r)) -> ([j], r) -> r
doAllJobs = doAllJobs' []

-- Similar to doAllJobs, with an explicit done job list.
doAllJobs' :: (Eq j, Semigroup r) => [j] -> (j -> ([j], r)) -> ([j], r) -> r
doAllJobs' doneList doJob ((j:js), r)
  | j `elem` doneList
  = doAllJobs' doneList doJob (js, r)
  | otherwise
  = doAllJobs' (j:doneList) doJob (js' ++ js, r' <> r)
  where (js', r') = doJob j
doAllJobs' _ _ ([], r) = r -- All job done, just return the result
