{-# OPTIONS_GHC -O2 -fno-full-laziness -fno-prof-auto #-}

module ParkBench.BenchmarkInternal
  ( whnf,
    whnfIO,
  )
where

import Data.Word (Word64)
import Prelude

whnf :: (a -> b) -> a -> Word64 -> IO ()
whnf f x =
  go
  where
    go :: Word64 -> IO ()
    go = \case
      0 -> pure ()
      -- `f x` won't be let-floated out with -fno-full-laziness
      n -> f x `seq` go (n -1)
-- prevent `whnf f x` from inlining to prevent `f x` from getting let-floated
{-# NOINLINE whnf #-}

whnfIO :: IO a -> Word64 -> IO ()
whnfIO io =
  go
  where
    go :: Word64 -> IO ()
    go = \case
      0 -> pure ()
      n -> do
        result <- io
        result `seq` go (n -1)
{-# NOINLINE whnfIO #-}
