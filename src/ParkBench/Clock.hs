module ParkBench.Clock
  ( Seconds,
    oneMillisecond,
    cpuPrecision,
  )
where

import Data.Ratio ((%))
import ParkBench.Prelude
import System.Clock
import System.IO.Unsafe (unsafePerformIO)

type Seconds = Rational

oneMillisecond :: Seconds
oneMillisecond =
  1 / 1000

cpuPrecision :: Seconds
cpuPrecision =
  unsafePerformIO do
    TimeSpec 0 ns <- getRes ProcessCPUTime
    pure (fromIntegral ns % 1_000_000_000)
