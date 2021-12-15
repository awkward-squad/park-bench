-- | Text builder helpers.
--
-- Code for printing numbers in 3 or 4 characters adapted from `tasty-bench`:
--   https://github.com/Bodigrim/tasty-bench/blob/412ae68dbd9582e0b19ff5f2c7f9ead25e104cce/Test/Tasty/Bench.hs#L795
module ParkBench.Builder
  ( Builder,
    build,
    bytes4,
    c,
    cs,
    Builder.decimal,
    ParkBench.Builder.empty,
    multiplier,
    nanos3,
    nanos4,
    ParkBench.Builder.null,
    percentage,
    sepBy,
    rational4,
    t,
  )
where

import qualified Data.List as List
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder (decimal)
import qualified Data.Text.Lazy.Builder.RealFloat as Builder
import ParkBench.Prelude

build :: Builder -> Text
build =
  LazyText.toStrict . Builder.toLazyText

-- | Render nanoseconds, trying to fit into 4 characters.
bytes4 :: Rational -> Builder
bytes4 (r2d -> b)
  | b < 0.5 = ParkBench.Builder.empty
  | b < 995 = double 0 b <> " b"
  | b < 9_950 = double 2 kb <> " kb"
  | b < 99_500 = double 1 kb <> " kb"
  | b < 995_000 = double 0 kb <> " kb"
  | b < 9_950_000 = double 2 mb <> " mb"
  | b < 99_500_000 = double 1 mb <> " mb"
  | b < 995_000_000 = double 0 mb <> " mb"
  | b < 9_950_000_000 = double 2 gb <> " gb"
  | b < 99_500_000_000 = double 1 gb <> " gb"
  | otherwise = double 0 gb <> " gb"
  where
    kb = b / 1_000
    mb = b / 1_000_000
    gb = b / 1_000_000_000

c :: Char -> Builder
c =
  Builder.singleton
{-# INLINE c #-}

cs :: Int -> Char -> Builder
cs n =
  mconcat . replicate n . Builder.singleton

double :: Int -> Double -> Builder
double i =
  Builder.formatRealFloat Builder.Fixed (Just i)

empty :: Builder
empty =
  mempty
{-# INLINE empty #-}

multiplier :: Rational -> Builder
multiplier (r2d -> n)
  | a < 0.005 = ParkBench.Builder.empty
  | a < 9.95 = double 2 n <> "x"
  | a < 99.5 = double 1 n <> "x"
  | otherwise = double 0 n <> "x"
  where
    a = abs n

-- | Render nanoseconds, trying to fit into 3 characters.
nanos3 :: Rational -> Builder
nanos3 (r2d -> ns)
  | ns < 0.5 = ParkBench.Builder.empty
  | ns < 995 = double 0 ns <> " ns"
  | ns < 9_950 = double 1 us <> " µs"
  | ns < 995_000 = double 0 us <> " µs"
  | ns < 9_950_000 = double 1 ms <> " ms"
  | ns < 995_000_000 = double 0 ms <> " ms"
  | ns < 9_950_000_000 = double 1 s <> " s"
  | otherwise = double 0 s <> " s"
  where
    us = ns / 1_000
    ms = ns / 1_000_000
    s = ns / 1_000_000_000

-- | Render nanoseconds, trying to fit into 4 characters.
nanos4 :: Rational -> Builder
nanos4 (r2d -> ns)
  | ns < 0.5 = ParkBench.Builder.empty
  | ns < 995 = double 0 ns <> " ns"
  | ns < 9_950 = double 2 us <> " µs"
  | ns < 99_500 = double 1 us <> " µs"
  | ns < 995_000 = double 0 us <> " µs"
  | ns < 9_950_000 = double 2 ms <> " ms"
  | ns < 99_500_000 = double 1 ms <> " ms"
  | ns < 995_000_000 = double 0 ms <> " ms"
  | ns < 9_950_000_000 = double 2 s <> " s"
  | ns < 99_500_000_000 = double 1 s <> " s"
  | otherwise = double 0 s <> " s"
  where
    us = ns / 1_000
    ms = ns / 1_000_000
    s = ns / 1_000_000_000

-- | /O(n)/.
null :: Builder -> Bool
null =
  LazyText.null . Builder.toLazyText

percentage :: Rational -> Builder
percentage ((* 100) -> n)
  | a < 5 / 1000 = ParkBench.Builder.empty
  | a < 995 / 100 = double 2 d <> "%"
  | a < 100 = double 1 d <> "%"
  | otherwise = double 0 d <> "%"
  where
    a = abs n
    d = r2d n

-- | Render a rational, trying to fit into 4 characters.
rational4 :: Rational -> Builder
rational4 (r2d -> d)
  | a < 0.005 = ParkBench.Builder.empty
  | a < 9.95 = double 2 d
  | a < 99.5 = double 1 d
  | otherwise = double 0 d
  where
    a = abs d

sepBy :: [Builder] -> Builder -> Builder
sepBy xs x =
  mconcat (List.intersperse x xs)

t :: Text -> Builder
t =
  Builder.fromText
{-# INLINE t #-}
