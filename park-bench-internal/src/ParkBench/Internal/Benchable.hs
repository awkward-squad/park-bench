module ParkBench.Internal.Benchable
  ( Benchable,
    run,
    mapIO,
    function,
    action,
  )
where

import qualified ParkBench.Internal.Benchable.Internal as Internal
import ParkBench.Internal.Prelude

-- | A benchmarkable thing, constructed with either 'function' or 'action'.
newtype Benchable a
  = Benchable (Word64 -> IO a)

-- | Run a benchmarkable thing for a number of iterations.
run :: Benchable a -> Word64 -> IO a
run =
  coerce
{-# INLINE run #-}

-- | Map over a benchmarkable thing in @IO@.
mapIO :: (IO a -> IO b) -> Benchable a -> Benchable b
mapIO =
  \f (Benchable g) ->
    Benchable (f . g)
{-# INLINE mapIO #-}

function :: (a -> b) -> a -> Benchable ()
function =
  \f x -> Benchable (Internal.function f x)
{-# INLINE function #-}

action :: IO a -> Benchable ()
action =
  \x -> Benchable (Internal.action x)
{-# INLINE action #-}
