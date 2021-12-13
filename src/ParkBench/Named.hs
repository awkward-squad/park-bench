module ParkBench.Named
  ( Named (..),
  )
where

import ParkBench.Prelude

-- | A named thing.
data Named a = Named
  { name :: String,
    thing :: a
  }
  deriving stock (Foldable, Functor, Traversable)
