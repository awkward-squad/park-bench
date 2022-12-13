module ParkBench.Array1
  ( Array1
  ) where

import ParkBench.Prelude
import Data.Array (Array)

newtype Array1 a =
  Array1 (Array Int a)
