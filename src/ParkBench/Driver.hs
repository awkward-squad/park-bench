module ParkBench.Driver
  ( benchmark1,
    Pull1,
    pull1,
    benchmark,
    Pull,
    Pulls,
    pulls,
    pull,
  )
where

import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import ParkBench.Benchable (Benchable)
import qualified ParkBench.Benchable as Benchable
import ParkBench.Prelude
import ParkBench.RtsStats (RtsStats)
import ParkBench.Statistics

newtype Pull1 a
  = Pull1 (IO (Estimate a, Pull1 a))

pull1 :: Pull1 a -> IO (Estimate a, Pull1 a)
pull1 =
  coerce
{-# INLINE pull1 #-}

-- | Like 'benchmark', but optimized for only running one benchmark.
benchmark1 :: forall a. Roll a => Benchable (Timed a) -> Pull1 a
benchmark1 benchable = do
  Pull1 do
    firstTime <- Benchable.run benchable 1
    pure (go (initialEstimate firstTime))
  where
    go :: Estimate a -> (Estimate a, Pull1 a)
    go oldEstimate =
      ( oldEstimate,
        Pull1 do
          let newIters = itersInOneTenthOfASecond oldEstimate
          newTime <- Benchable.run benchable newIters
          pure (go (updateEstimate newIters newTime oldEstimate))
      )
{-# SPECIALIZE benchmark1 :: Benchable (Timed RtsStats) -> Pull1 RtsStats #-}

benchmark :: forall a. Roll a => Benchable (Timed a) -> IO (IO (Estimate a), Pull a)
benchmark benchable = do
  e0 <- initialEstimate <$> Benchable.run benchable 1
  estimateRef <- newIORef e0
  let go :: Estimate a -> Pull a
      go oldEstimate =
        Pull (elapsed oldEstimate) do
          let newIters = itersInOneTenthOfASecond oldEstimate
          newTime <- Benchable.run benchable newIters
          let !newEstimate = updateEstimate newIters newTime oldEstimate
          writeIORef estimateRef newEstimate
          pure (go newEstimate)
  pure (readIORef estimateRef, go e0)
{-# SPECIALIZE benchmark :: Benchable (Timed RtsStats) -> IO (IO (Estimate RtsStats), Pull RtsStats) #-}

-- Given this latest estimate, how many iters could we run in 0.1 seconds?
itersInOneTenthOfASecond :: Estimate a -> Word64
itersInOneTenthOfASecond Estimate {mean, samples} =
  max 1 (min samples (floor (oneTenthOfASecond / nanoseconds mean)))
  where
    oneTenthOfASecond :: Rational
    oneTenthOfASecond = 100_000_000

data Pull a
  = Pull
      -- amount of time this pull has gotten
      {-# UNPACK #-} !Rational
      !(IO (Pull a))

isMoreUrgentThan :: Pull a -> Pull a -> Bool
Pull t0 _ `isMoreUrgentThan` Pull t1 _ =
  t0 < t1

-- | A @Pulls@ represents the suspended state of a collection of 1+ benchmarks.
data Pulls a
  = -- Most benchmark runs are probably only comparing 1-3 things, so we optimize those cases.
    P1 !(Pull a)
  | P2 !(Pull a) !(Pull a)
  | P3 !(Pull a) !(Pull a) !(Pull a)
  | -- invariant: 4+ elements
    Pn_ ![Pull a]

pattern Pn :: Pull a -> [Pull a] -> Pulls a
pattern Pn p ps <- Pn_ (p : ps)

{-# COMPLETE P1, P2, P3, Pn #-}

-- | Construct a 'Pulls' from a non-empty list of 'Pull'.
pulls :: NonEmpty (Pull a) -> Pulls a
pulls =
  pulls' . NonEmpty.sortWith \(Pull t _) -> t

pulls' :: NonEmpty (Pull a) -> Pulls a
pulls' = \case
  a :| [] -> P1 a
  a :| [b] -> P2 a b
  a :| [b, c] -> P3 a b c
  a :| as -> Pn_ (a : as)

-- | Pull on a 'Pulls', which blocks until the benchmark that has heretofore accumulated the smallest amount of runtime
-- runs once more.
--
-- Returns the 'Pulls' to use next time, which reflects the latest benchmark run that just completed.
pull :: Pulls a -> IO (Pulls a)
pull = \case
  P1 (Pull _ p0) -> do
    p <- p0
    pure (P1 p)
  P2 (Pull _ p0) q -> do
    p <- p0
    pure
      if q `isMoreUrgentThan` p
        then P2 q p
        else P2 p q
  P3 (Pull _ p0) q r -> do
    p <- p0
    pure
      if q `isMoreUrgentThan` p
        then
          if r `isMoreUrgentThan` p
            then P3 q r p
            else P3 q p r
        else P3 p q r
  Pn (Pull _ p0) ps -> do
    p <- p0
    pure (Pn_ (insertPull p ps))

insertPull :: Pull a -> [Pull a] -> [Pull a]
insertPull p0 = \case
  [] -> [p0]
  p1 : ps ->
    if p0 `isMoreUrgentThan` p1
      then p0 : p1 : ps
      else p1 : insertPull p0 ps
