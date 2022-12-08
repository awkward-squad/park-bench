-- | Internal guts, dragons, gargoyles, etc. This module does not follow the PVP.
module ParkBench.Internal
  ( -- * Benchmarking
    benchmark1,
    Pull1 (..),
    benchmark,
    Pull,
    Pulls,
    pulls,
    pull,

    -- ** Benchable
    Benchable,
    mapIO,
    run,
    whnf,
    whnfIO,

    -- * Measurement
    measure,

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

    -- * Table rendering

    -- ** High-level row/cell machinery
    R (..),
    Cellular (..),
    BytesCell (..),
    BytesPerSecondCell (..),
    IncomparableWord3Cell (..),
    IncomparablePercentageCell (..),
    NanosecondsCell (..),
    NumberCell (..),
    NumberCell' (..),
    PercentageCell (..),
    PercentageCell' (..),
    rowMaker,

    -- ** Table machinery
    Table (..),
    renderTable,
    RowGroup (..),
    Row (..),
    Cell (..),
    isEmptyCell,
    Color (..),
  )
where

import ParkBench.Benchable
import ParkBench.Driver
import ParkBench.Measure
import ParkBench.Pretty
import ParkBench.RtsStats
import ParkBench.Statistics
