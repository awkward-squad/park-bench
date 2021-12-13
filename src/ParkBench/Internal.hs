module ParkBench.Internal
  ( -- * Benchmarking
    benchmark,
    Pull,
    pull,
    function,
    action,

    -- * Statistics
    Timed (..),
    Estimate (..),
    initialEstimate,
    updateEstimate,
    stdev,
    variance,
    Roll (..),

    -- * RTS stats
    RtsStats (..),
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

import ParkBench.Benchmark (action, function)
import ParkBench.RtsStats
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
import ParkBench.Statistics
  ( Estimate (..),
    Pull,
    Roll (..),
    Timed (..),
    benchmark,
    initialEstimate,
    pull,
    stdev,
    updateEstimate,
    variance,
  )
