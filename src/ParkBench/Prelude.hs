module ParkBench.Prelude
  ( Seconds,
    longzip,
    read,
    module X,
  )
where

import Control.Applicative as X
import Control.Monad as X
import Data.Coerce as X (coerce)
import Data.Foldable as X (fold, foldl')
import Data.Proxy as X (Proxy (..))
import Data.Typeable (showsTypeRep, typeRep)
import Data.Typeable as X (Typeable)
import Data.Word as X (Word64)
import GHC.Generics as X (Generic)
import Text.Read (readMaybe)
import Prelude as X hiding (read)

type Seconds = Rational

longzip :: (a -> a -> a) -> [a] -> [a] -> [a]
longzip _ [] ys = ys
longzip _ xs [] = xs
longzip f (x : xs) (y : ys) = f x y : longzip f xs ys

read :: forall a. (Read a, Typeable a) => String -> a
read s =
  case readMaybe s of
    Nothing -> error ("Cannot parse " ++ show s ++ " as a(n) " ++ showsTypeRep (typeRep (Proxy @a)) "")
    Just x -> x
