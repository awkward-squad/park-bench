module ParkBench.Prelude
  ( divide,
    divide',
    w2r,
    module X,
  )
where

import Control.Applicative as X
import Control.Monad as X
import Data.Coerce as X (coerce)
import Data.Foldable as X (fold, foldl')
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

w2r :: Word64 -> Rational
w2r = fromIntegral
