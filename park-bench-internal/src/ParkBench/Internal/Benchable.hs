module ParkBench.Internal.Benchable
  ( Benchable,
    run,
    mapIO,
    whnf,
    whnfIO,
  )
where

import qualified ParkBench.Internal.Benchable.Internal as Internal
import ParkBench.Internal.Prelude

newtype Benchable a
  = Benchable (Word64 -> IO a)

run :: Benchable a -> Word64 -> IO a
run =
  coerce
{-# INLINE run #-}

mapIO :: (IO a -> IO b) -> Benchable a -> Benchable b
mapIO f (Benchable g) =
  Benchable (f . g)
{-# INLINE mapIO #-}

whnf :: forall a b. (a -> b) -> a -> Benchable ()
whnf =
  coerce @((a -> b) -> a -> Word64 -> IO ()) Internal.whnf
{-# INLINE whnf #-}

whnfIO :: forall a. IO a -> Benchable ()
whnfIO =
  coerce @(IO a -> Word64 -> IO ()) Internal.whnfIO
{-# INLINE whnfIO #-}
