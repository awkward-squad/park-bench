-- | RTS stats type.
module ParkBench.RtsStats
  ( RtsStats (..),
    allocated_bytes_per_second,
    average_live_data,
    gc_average_ns,
    gc_cpu_percent,
    gc_wall_percent,
    max_normal_objects_bytes,
    minor_gcs,
    mut_cpu_percent,
    mut_wall_percent,
    work_balance,
  )
where

import ParkBench.Prelude
import ParkBench.Statistics

-- TODO nonmoving_gc_*
data RtsStats = RtsStats
  { allocated_bytes :: {-# UNPACK #-} !Rational,
    copied_bytes :: {-# UNPACK #-} !Rational,
    cpu_ns :: {-# UNPACK #-} !Rational,
    -- requires major gc
    cumulative_live_bytes :: {-# UNPACK #-} !Rational,
    cumulative_par_balanced_copied_bytes :: {-# UNPACK #-} !Rational,
    elapsed_ns :: {-# UNPACK #-} !Rational,
    gc_cpu_ns :: {-# UNPACK #-} !Rational,
    gc_elapsed_ns :: {-# UNPACK #-} !Rational,
    gcs :: {-# UNPACK #-} !Rational,
    major_gcs :: {-# UNPACK #-} !Rational,
    -- requires major gc
    max_compact_bytes :: {-# UNPACK #-} !Rational,
    -- requires major gc
    max_large_objects_bytes :: {-# UNPACK #-} !Rational,
    -- requires major gc
    max_live_bytes :: {-# UNPACK #-} !Rational,
    max_mem_in_use_bytes :: {-# UNPACK #-} !Rational,
    -- requires major gc
    max_slop_bytes :: {-# UNPACK #-} !Rational,
    mutator_cpu_ns :: {-# UNPACK #-} !Rational,
    mutator_elapsed_ns :: {-# UNPACK #-} !Rational,
    par_copied_bytes :: {-# UNPACK #-} !Rational
  }
  deriving stock (Show)

instance Roll RtsStats where
  roll step s0 s1 =
    RtsStats
      { allocated_bytes = averagey allocated_bytes,
        copied_bytes = averagey copied_bytes,
        cpu_ns = averagey cpu_ns,
        cumulative_live_bytes = averagey cumulative_live_bytes,
        cumulative_par_balanced_copied_bytes = averagey cumulative_par_balanced_copied_bytes,
        elapsed_ns = averagey elapsed_ns,
        gc_cpu_ns = averagey gc_cpu_ns,
        gc_elapsed_ns = averagey gc_elapsed_ns,
        gcs = averagey gcs,
        major_gcs = averagey major_gcs,
        max_compact_bytes = maxey max_compact_bytes,
        max_large_objects_bytes = maxey max_large_objects_bytes,
        max_live_bytes = maxey max_live_bytes,
        max_mem_in_use_bytes = maxey max_mem_in_use_bytes,
        max_slop_bytes = maxey max_slop_bytes,
        mutator_cpu_ns = averagey mutator_cpu_ns,
        mutator_elapsed_ns = averagey mutator_elapsed_ns,
        par_copied_bytes = averagey par_copied_bytes
      }
    where
      averagey :: (RtsStats -> Rational) -> Rational
      averagey f =
        step (f s0) (f s1)

      maxey :: (RtsStats -> Rational) -> Rational
      maxey f =
        max (f s0) (f s1)

allocated_bytes_per_second :: RtsStats -> Rational
allocated_bytes_per_second s =
  allocated_bytes s `divide` (elapsed_ns s / 1_000_000_000)

-- requires major gc
average_live_data :: RtsStats -> Rational
average_live_data s =
  cumulative_live_bytes s `divide` major_gcs s

gc_average_ns :: RtsStats -> Rational
gc_average_ns s =
  gcs s `divide` gc_elapsed_ns s

gc_cpu_percent :: RtsStats -> Rational
gc_cpu_percent s =
  gc_cpu_ns s `divide` cpu_ns s

gc_wall_percent :: RtsStats -> Rational
gc_wall_percent s =
  gc_elapsed_ns s `divide` elapsed_ns s

-- requires major gc
max_normal_objects_bytes :: RtsStats -> Rational
max_normal_objects_bytes s =
  max_live_bytes s - max_compact_bytes s - max_large_objects_bytes s

minor_gcs :: RtsStats -> Rational
minor_gcs s =
  gcs s - major_gcs s

mut_cpu_percent :: RtsStats -> Rational
mut_cpu_percent s =
  mutator_cpu_ns s `divide` cpu_ns s

mut_wall_percent :: RtsStats -> Rational
mut_wall_percent s =
  mutator_elapsed_ns s `divide` elapsed_ns s

work_balance :: RtsStats -> Maybe Rational
work_balance s =
  cumulative_par_balanced_copied_bytes s `divide'` par_copied_bytes s
