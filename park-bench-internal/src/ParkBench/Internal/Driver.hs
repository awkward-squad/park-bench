module ParkBench.Internal.Driver
  ( benchmark1,
    Pull1,
    pull1,
    benchmark,
    Pull,
    Pulls,
    makePulls,
    pull,
  )
where

import Data.Foldable (toList)
import Data.IORef
import ParkBench.Internal.Array1 (Array1)
import qualified ParkBench.Internal.Array1 as Array1
import ParkBench.Internal.Benchable (Benchable)
import qualified ParkBench.Internal.Benchable as Benchable
import ParkBench.Internal.Prelude
import ParkBench.Internal.RtsStats (RtsStats)
import ParkBench.Internal.Statistics

newtype Pull1 a
  = Pull1 (IO (Estimate a, Pull1 a))

pull1 :: Pull1 a -> IO (Estimate a, Pull1 a)
pull1 =
  coerce
{-# INLINE pull1 #-}

-- | Like 'benchmark', but optimized for only running one benchmark.
benchmark1 :: forall a. Roll a => Rational -> Benchable (Timed a) -> Pull1 a
benchmark1 nanos benchable = do
  Pull1 do
    firstTime <- Benchable.run benchable 1
    pure (go (initialEstimate firstTime))
  where
    go :: Estimate a -> (Estimate a, Pull1 a)
    go oldEstimate =
      ( oldEstimate,
        Pull1 do
          let newIters = itersInNanoseconds oldEstimate nanos
          newTime <- Benchable.run benchable newIters
          pure (go (updateEstimate newIters newTime oldEstimate))
      )
{-# SPECIALIZE benchmark1 :: Rational -> Benchable (Timed RtsStats) -> Pull1 RtsStats #-}

benchmark :: forall a. Roll a => Rational -> Benchable (Timed a) -> IO (IO (Estimate a), Pull a)
benchmark nanos benchable = do
  e0 <- initialEstimate <$> Benchable.run benchable 1
  estimateRef <- newIORef e0
  let go :: Estimate a -> Pull a
      go oldEstimate =
        Pull (elapsed oldEstimate) do
          let newIters = itersInNanoseconds oldEstimate nanos
          newTime <- Benchable.run benchable newIters
          let !newEstimate = updateEstimate newIters newTime oldEstimate
          writeIORef estimateRef newEstimate
          pure (go newEstimate)
  pure (readIORef estimateRef, go e0)
{-# SPECIALIZE benchmark :: Rational -> Benchable (Timed RtsStats) -> IO (IO (Estimate RtsStats), Pull RtsStats) #-}

-- Given this latest estimate, how many iters could we run in the given number of nanoseconds?
itersInNanoseconds :: Estimate a -> Rational -> Word64
itersInNanoseconds Estimate {mean, samples} nanos =
  max 1 (min samples (floor (nanos / nanoseconds mean)))

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
makePulls :: Array1 (Pull a) -> Pulls a
makePulls xs
  | n == 1 = P1 (Array1.get 0 xs)
  | n == 2 = P2 (Array1.get 0 xs) (Array1.get 1 xs)
  | n == 3 = P3 (Array1.get 0 xs) (Array1.get 1 xs) (Array1.get 2 xs)
  | otherwise = Pn_ (toList xs)
  where
    n = length xs

-- | Pull on a 'Pulls', which blocks until the benchmark that has heretofore accumulated the smallest amount of runtime
-- runs once more.
--
-- Returns the 'Pulls' to use next time, which reflects the latest benchmark run that just completed.
pull :: Pulls a -> IO (Pulls a)
pull = \case
  P1 (Pull _ action) -> do
    x0 <- action
    pure (P1 x0)
  P2 (Pull _ action) x1 -> do
    x0 <- action
    pure
      if x1 `isMoreUrgentThan` x0
        then P2 x1 x0
        else P2 x0 x1
  P3 (Pull _ action) x1 x2 -> do
    x0 <- action
    pure
      if x1 `isMoreUrgentThan` x0
        then
          if x2 `isMoreUrgentThan` x0
            then P3 x1 x2 x0
            else P3 x1 x0 x2
        else P3 x0 x1 x2
  Pn (Pull _ action) xs -> do
    x0 <- action
    pure (Pn_ (insertPull x0 xs))

insertPull :: Pull a -> [Pull a] -> [Pull a]
insertPull x0 = \case
  [] -> [x0]
  x1 : xs ->
    if x0 `isMoreUrgentThan` x1
      then x0 : x1 : xs
      else x1 : insertPull x0 xs
