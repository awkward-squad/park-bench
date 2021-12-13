module ParkBench.Benchmark
  ( function,
    action,
  )
where

import Control.Exception (evaluate)
import qualified GHC.Stats as GHC
import ParkBench.Prelude
import ParkBench.RtsStats
import ParkBench.Statistics (Timed (..))
import System.Mem (performGC)

timeit :: IO () -> IO (Timed RtsStats)
timeit act = do
  performGC
  s0 <- GHC.getRTSStats
  act
  s1 <- GHC.getRTSStats
  performGC
  s2 <- GHC.getRTSStats
  let measure :: Integral a => (GHC.RTSStats -> a) -> Rational
      measure f =
        toRational (f s1 - f s0)
  let measure2 :: Integral a => (GHC.RTSStats -> a) -> Rational
      measure2 f =
        toRational (f s2 - f s0)
  let gauge :: Integral a => (GHC.RTSStats -> a) -> Rational
      gauge f =
        toRational (max (f s1) (f s0))
  let gauge2 :: Integral a => (GHC.RTSStats -> a) -> Rational
      gauge2 f =
        toRational (max (f s2) (f s0))
  let elapsed_ns :: Rational
      elapsed_ns =
        measure GHC.elapsed_ns
  pure
    Timed
      { nanoseconds = elapsed_ns,
        value =
          RtsStats
            { allocated_bytes = measure GHC.allocated_bytes,
              copied_bytes = measure GHC.copied_bytes,
              cpu_ns = measure GHC.cpu_ns,
              cumulative_live_bytes = measure2 GHC.cumulative_live_bytes,
              cumulative_par_balanced_copied_bytes = measure GHC.cumulative_par_balanced_copied_bytes,
              elapsed_ns,
              gc_cpu_ns = measure GHC.gc_cpu_ns,
              gc_elapsed_ns = measure GHC.gc_elapsed_ns,
              gcs = measure GHC.gcs,
              major_gcs = measure GHC.major_gcs,
              max_compact_bytes = gauge2 GHC.max_compact_bytes,
              max_large_objects_bytes = gauge2 GHC.max_large_objects_bytes,
              max_live_bytes = gauge2 GHC.max_live_bytes,
              max_mem_in_use_bytes = gauge GHC.max_mem_in_use_bytes,
              max_slop_bytes = gauge2 GHC.max_slop_bytes,
              mutator_cpu_ns = measure GHC.mutator_cpu_ns,
              mutator_elapsed_ns = measure GHC.mutator_elapsed_ns,
              par_copied_bytes = measure GHC.par_copied_bytes
            }
      }

function :: (forall r. r -> a -> b) -> a -> Word64 -> IO (Timed RtsStats)
function f x =
  timeit . go
  where
    go :: Word64 -> IO ()
    go = \case
      0 -> pure ()
      n -> do
        -- problem with `f x`: it could be let-floated
        _ <- evaluate (f n x)
        go (n -1)
{-# NOINLINE function #-}

action :: IO a -> Word64 -> IO (Timed RtsStats)
action x =
  timeit . go
  where
    go :: Word64 -> IO ()
    go = \case
      0 -> pure ()
      n -> do
        _ <- evaluate =<< x
        go (n -1)
