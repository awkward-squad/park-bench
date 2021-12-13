module ParkBench.Builder
  ( Builder,
    build,
    c,
    cs,
    Builder.decimal,
    ParkBench.Builder.empty,
    t,
    ParkBench.Builder.null,
    sepBy,
    rational3,
  )
where

import qualified Data.List as List
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder (decimal)
import qualified Data.Text.Lazy.Builder.RealFloat as Builder
import Numeric (showFFloat)
import ParkBench.Prelude

build :: Builder -> Text
build =
  LazyText.toStrict . Builder.toLazyText

c :: Char -> Builder
c =
  Builder.singleton
{-# INLINE c #-}

cs :: Int -> Char -> Builder
cs n =
  mconcat . replicate n . Builder.singleton

empty :: Builder
empty =
  mempty
{-# INLINE empty #-}

-- | /O(n)/.
null :: Builder -> Bool
null =
  LazyText.null . Builder.toLazyText

-- | Build a rational number to at least three significant figures, for some non-standard definition of significant
-- figure that I made up. Near-zero gets rendered as the empty string.
rational3 :: Rational -> Builder
rational3 n
  | na >= 100 = Builder.formatRealFloat Builder.Fixed (Just 0) nd
  | na >= 10 = Builder.formatRealFloat Builder.Fixed (Just 1) nd
  | na >= 1 = Builder.formatRealFloat Builder.Fixed (Just 2) nd
  | otherwise =
    case showFFloat (Just 3) nd "" of
      "0.000" -> ""
      "-0.000" -> ""
      s -> Builder.fromString s
  where
    na = abs n
    nd = realToFrac n :: Double

sepBy :: [Builder] -> Builder -> Builder
sepBy xs x =
  mconcat (List.intersperse x xs)

t :: Text -> Builder
t =
  Builder.fromText
{-# INLINE t #-}
