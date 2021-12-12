-- Basic technique for determining when an estimate is good enough is cribbed from
-- https://github.com/Bodigrim/tasty-bench
module ParkBench.Statistics
  ( benchmark,
    Pull,
    pull,
    Estimate (..),
    goodness,
    Sample (..),
    Scaled (..),
    Timed (..),
    fit,
  )
where

import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import ParkBench.Prelude

-- | A value that took a certan time to compute.
data Timed a = Timed
  { time :: !Seconds,
    -- the value is intentionally lazy so we don't waste time computing it only to throw it away due to an
    -- insufficiently accurate measurement (i.e. stdev too high).
    value :: a
  }
  deriving stock (Functor, Show)

instance Monoid a => Monoid (Timed a) where
  mempty = Timed 0 mempty
  mappend = (<>)

instance Semigroup a => Semigroup (Timed a) where
  Timed n0 x0 <> Timed n1 x1 =
    Timed (n0 + n1) (x0 <> x1)

data Estimate a = Estimate
  { mean :: !(Timed a),
    stdev :: !Double
  }
  deriving stock (Functor, Show)

-- | A measure of goodness of the estimate, from 0 to 1.
goodness :: Estimate a -> Double
goodness (Estimate (Timed m _) s) =
  1 - (s / realToFrac m)

estimate :: Sample a => Timed a -> Timed a -> Estimate a
estimate (Timed t0 x0) (Timed t1 x1) =
  Estimate
    { mean = Timed m (combine2 x0 x1),
      stdev = std t0 m t1
    }
  where
    m :: Rational
    m =
      fit t0 t1

    std :: Rational -> Rational -> Rational -> Double
    std x y z =
      sqrt (realToFrac (sqr (x - y) + sqr (z - 2 * y)))
      where
        sqr :: Rational -> Rational
        sqr n =
          n * n

class Scaled a where
  downscale :: Word64 -> a -> a

instance Scaled Double where
  downscale n w = w / fromIntegral n

instance Scaled Rational where
  downscale n w = w / fromIntegral n

instance Scaled a => Scaled (Maybe a) where
  downscale n = fmap (downscale n)

instance Scaled a => Scaled (Estimate a) where
  downscale n (Estimate m s) = Estimate (downscale n m) (downscale n s)

instance Scaled a => Scaled (Timed a) where
  downscale n (Timed m x) = Timed (downscale n m) (downscale n x)

class Sample a where
  -- | Combine two samples, where the second is of twice as many iterations as the first.
  combine2 :: a -> a -> a

data Pull a
  = Pull !Double (IO (Pull a))

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
benchmark :: forall a. (Sample a, Scaled a) => (Word64 -> IO (Timed a)) -> IO (IO (Estimate a), Pull a)
benchmark run = do
  t <- run 1
  ref <- newIORef (Estimate t 0)

  let another :: Word64 -> Timed a -> IO (Pull a)
      another n t1 = do
        t2 <- run (2 * n)
        let e = estimate t1 t2
        writeIORef ref (downscale n e)
        pure (Pull (goodness e) (another (2 * n) t2))

  pure (readIORef ref, Pull 1 (another 1 t))

fit :: Rational -> Rational -> Rational
fit x1 x2 =
  (x1 / 5) + (2 * x2 / 5)
