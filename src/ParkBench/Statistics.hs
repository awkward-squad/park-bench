module ParkBench.Statistics
  ( Timed (..),
    Estimate (..),
    initialEstimate,
    updateEstimate,
    stdev,
    variance,
    Roll (..),
    benchmark,
    Pull,
    pull,
  )
where

import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import ParkBench.Prelude

-- | A value that took a certan time to compute.
data Timed a = Timed
  { nanoseconds :: {-# UNPACK #-} !Rational,
    value :: !a
  }
  deriving stock (Functor, Show)

instance Monoid a => Monoid (Timed a) where
  mempty = Timed 0 mempty
  mappend = (<>)

instance Semigroup a => Semigroup (Timed a) where
  Timed n0 x0 <> Timed n1 x1 =
    Timed (n0 + n1) (x0 <> x1)

data Estimate a = Estimate
  { kvariance :: {-# UNPACK #-} !Rational,
    mean :: {-# UNPACK #-} !(Timed a),
    samples :: !Natural
  }
  deriving stock (Functor, Show)

stdev :: Estimate a -> Double
stdev =
  sqrt . (fromRational @Double) . variance

variance :: Estimate a -> Rational
variance (Estimate kvariance _ samples) =
  if samples == 1
    then 0
    else kvariance / n2r (samples - 1)

-- | @updateEstimate v@ creates an estimate per thing-that-took-time @v@ that was a run of 1 iteration.
initialEstimate :: Timed a -> Estimate a
initialEstimate mean =
  Estimate
    { kvariance = 0,
      mean,
      samples = 1
    }

-- | @updateEstimate n v e@ updates estimate @e@ per thing-that-took-time @v@ that was a run of @n@ iterations.
updateEstimate :: Roll a => Word64 -> Timed a -> Estimate a -> Estimate a
updateEstimate n (Timed tn value1) (Estimate kvariance (Timed mean value0) samples) =
  Estimate kvariance' (Timed mean' value') samples'
  where
    kvariance' = kvariance + nr * (t1 - mean) * (t1 - mean')
    mean' = rollmean mean tn
    samples' = samples + w2n n
    samplesr' = n2r samples'
    t1 = tn / nr
    value' = roll rollmean value0 value1
    rollmean u0 u1 = u0 + ((u1 - nr * u0) / samplesr')
    nr = w2r n

class Roll a where
  roll :: (Rational -> Rational -> Rational) -> a -> a -> a

data Pull a
  = Pull
      -- kvariance of latest estimate
      {-# UNPACK #-} !Rational
      (IO (Pull a))

pull :: NonEmpty (Pull a) -> IO (NonEmpty (Pull a))
pull (Pull _ p0 :| ps) = do
  p1 <- p0
  pure (insertPull p1 ps)

-- Insert a pull into a list of pulls in decreasing goodness order.
insertPull :: Pull a -> [Pull a] -> NonEmpty (Pull a)
insertPull p ps =
  NonEmpty.fromList (insertPull' p ps)

insertPull' :: Pull a -> [Pull a] -> [Pull a]
insertPull' p0@(Pull n _) = \case
  [] -> [p0]
  p1@(Pull m _) : ps ->
    if n <= m
      then p0 : p1 : ps
      else p1 : insertPull' p0 ps

-- | Benchmark forever, providing better and better estimates.
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
        pure (Pull (kvariance e0) (another e1))
        where
          n = next e0

  pure (readIORef ref, Pull 0 (another e))
  where
    -- target runs that take 0.1 seconds (e.g. 500_000_000 would be 0.5 seconds)
    next :: Estimate a -> Word64
    next Estimate {mean = Timed nanoseconds _, samples} =
      max 1 (min (n2w samples) (floor (100_000_000 / nanoseconds)))

n2r :: Natural -> Rational
n2r = fromIntegral

n2w :: Natural -> Word64
n2w = fromIntegral

w2n :: Word64 -> Natural
w2n = fromIntegral

w2r :: Word64 -> Rational
w2r = fromIntegral
