-- | This module contains helpers for benchmarking Haskell functions and IO actions.
module ParkBench.Benchmark
  ( whnf,
    whnfIO,
    measure,
  )
where

import qualified GHC.Stats as GHC
import ParkBench.BenchmarkInternal (whnf, whnfIO)
import ParkBench.Prelude
import ParkBench.RtsStats
import ParkBench.Statistics (Timed (..))
import System.Mem (performGC)

-- | Measure the time/memory usage of an IO action.
measure :: IO () -> IO (Timed RtsStats)
measure act = do
  performGC
  s0 <- GHC.getRTSStats
  act
  s1 <- GHC.getRTSStats
  -- Perform a major GC to update a few stats that are only accurate after a major GC.
  -- But the latest GC before collecting `s1` might have happened to be major, so check that first.
  s2 <-
    if GHC.gcdetails_gen (GHC.gc s0) == GHC.gcdetails_gen (GHC.gc s1)
      then pure s1
      else do
        performGC
        GHC.getRTSStats

  let elapsed_ns :: Rational
      elapsed_ns =
        toRational (GHC.elapsed_ns s1 - GHC.elapsed_ns s0)

  pure
    Timed
      { nanoseconds = elapsed_ns,
        value =
          RtsStats
            { allocated_bytes = toRational (GHC.allocated_bytes s1 - GHC.allocated_bytes s0),
              copied_bytes = toRational (GHC.copied_bytes s1 - GHC.copied_bytes s0),
              cpu_ns = toRational (GHC.cpu_ns s1 - GHC.cpu_ns s0),
              cumulative_live_bytes = toRational (GHC.cumulative_live_bytes s2 - GHC.cumulative_live_bytes s0),
              cumulative_par_balanced_copied_bytes =
                toRational (GHC.cumulative_par_balanced_copied_bytes s1 - GHC.cumulative_par_balanced_copied_bytes s0),
              elapsed_ns,
              gc_cpu_ns = toRational (GHC.gc_cpu_ns s1 - GHC.gc_cpu_ns s0),
              gc_elapsed_ns = toRational (GHC.gc_elapsed_ns s1 - GHC.gc_elapsed_ns s0),
              gcs = toRational (GHC.gcs s1 - GHC.gcs s0),
              major_gcs = toRational (GHC.major_gcs s1 - GHC.major_gcs s0),
              max_compact_bytes = toRational (max (GHC.max_compact_bytes s2) (GHC.max_compact_bytes s0)),
              max_large_objects_bytes =
                toRational (max (GHC.max_large_objects_bytes s2) (GHC.max_large_objects_bytes s0)),
              max_live_bytes = toRational (max (GHC.max_live_bytes s2) (GHC.max_live_bytes s0)),
              max_mem_in_use_bytes = toRational (max (GHC.max_mem_in_use_bytes s1) (GHC.max_mem_in_use_bytes s2)),
              max_slop_bytes = toRational (max (GHC.max_slop_bytes s2) (GHC.max_slop_bytes s0)),
              mutator_cpu_ns = toRational (GHC.mutator_cpu_ns s1 - GHC.mutator_cpu_ns s0),
              mutator_elapsed_ns = toRational (GHC.mutator_elapsed_ns s1 - GHC.mutator_elapsed_ns s0),
              par_copied_bytes = toRational (GHC.par_copied_bytes s1 - GHC.par_copied_bytes s0)
            }
      }
