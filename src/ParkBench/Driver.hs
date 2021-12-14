module ParkBench.Driver
  ( benchmark,
    Pull,
    pull,
  )
where

import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import ParkBench.Prelude
import ParkBench.RtsStats (RtsStats)
import ParkBench.Statistics

benchmark :: forall a. Roll a => (Word64 -> IO (Timed a)) -> IO (IO (Estimate a), Pull a)
benchmark run = do
  t <- run 1
  let e = initialEstimate t
  ref <- newIORef e

  let another :: Estimate a -> IO (Pull a)
      another e0 = do
        t2 <- run n
        let !e1 = updateEstimate n t2 e0
        writeIORef ref e1
        pure (Pull (w2r (samples e1) * nanoseconds (mean e1)) (another e1))
        where
          n = next e0

  pure (readIORef ref, Pull (nanoseconds (mean e)) (another e))
  where
    -- target runs that take 0.1 seconds (e.g. 500_000_000 would be 0.5 seconds)
    next :: Estimate a -> Word64
    next Estimate {mean = Timed nanoseconds _, samples} =
      max 1 (min samples (floor (100_000_000 / nanoseconds)))
{-# SPECIALIZE benchmark :: (Word64 -> IO (Timed RtsStats)) -> IO (IO (Estimate RtsStats), Pull RtsStats) #-}

data Pull a
  = Pull
      -- amount of time this pull has gotten
      {-# UNPACK #-} !Rational
      (IO (Pull a))

pull :: NonEmpty (Pull a) -> IO (NonEmpty (Pull a))
pull (Pull _ p0 :| ps) = do
  p1 <- p0
  pure (insertPull p1 ps)

-- Insert a pull into an ordered list of pulls, maintaining the invariant that the pull most in need of being run next
-- is first in the list.
insertPull :: Pull a -> [Pull a] -> NonEmpty (Pull a)
insertPull p ps =
  NonEmpty.fromList (insertPull' p ps)

insertPull' :: Pull a -> [Pull a] -> [Pull a]
insertPull' p0@(Pull t0 _) = \case
  [] -> [p0]
  p1@(Pull t1 _) : ps ->
    if t0 < t1
      then p0 : p1 : ps
      else p1 : insertPull' p0 ps
