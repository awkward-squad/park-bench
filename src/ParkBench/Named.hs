-- | Utility type.
module ParkBench.Named
  ( Named (..),
    name,
    thing,
  )
where

import ParkBench.Prelude

-- | A named thing.
data Named a
  = Named
      {-# UNPACK #-} !Text
      !a
  deriving stock (Foldable, Functor, Traversable)

name :: Named a -> Text
name (Named n _) =
  n

thing :: Named a -> a
thing (Named _ x) =
  x
