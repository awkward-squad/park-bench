-- | Simple statistics code.
module ParkBench.Statistics
  ( Timed (..),
    Estimate (..),
    initialEstimate,
    updateEstimate,
    stdev,
    variance,
    Roll (..),
  )
where

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
    samples :: {-# UNPACK #-} !Word64
  }
  deriving stock (Functor, Show)

stdev :: Estimate a -> Double
stdev =
  sqrt . (fromRational @Double) . variance

variance :: Estimate a -> Rational
variance (Estimate kvariance _ samples) =
  if samples == 1
    then 0
    else kvariance / w2r (samples - 1)

-- | @initialEstimate v@ creates an estimate per thing-that-took-time @v@ that was a run of 1 iteration.
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
    samples' = samples + n
    samplesr' = w2r samples'
    t1 = tn / nr
    value' = roll rollmean value0 value1
    rollmean u0 u1 = u0 + ((u1 - nr * u0) / samplesr')
    nr = w2r n

class Roll a where
  roll :: (Rational -> Rational -> Rational) -> a -> a -> a
