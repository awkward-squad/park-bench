module ParkBench.Prelude
  ( divide,
    divide',
    divideDouble,
    r2d,
    w2d,
    w2r,
    module X,
  )
where

import Control.Applicative as X
import Control.Monad as X
import Data.Coerce as X (coerce)
import Data.Foldable as X (fold, foldl')
import Data.Functor as X (($>))
import Data.Proxy as X (Proxy (..))
import Data.Text as X (Text)
import Data.Typeable as X (Typeable)
import Data.Word as X (Word64, Word8)
import GHC.Generics as X (Generic)
import Numeric.Natural as X (Natural)
import Prelude as X hiding (read)

divide :: Rational -> Rational -> Rational
divide n d =
  if d == 0 then 0 else n / d

divide' :: Rational -> Rational -> Maybe Rational
divide' n d =
  if d == 0 then Nothing else Just (n / d)

divideDouble :: Double -> Double -> Double
divideDouble x y =
  if y == 0 then 0 else x / y

r2d :: Rational -> Double
r2d = realToFrac

w2d :: Word64 -> Double
w2d = fromIntegral

w2r :: Word64 -> Rational
w2r = fromIntegral
