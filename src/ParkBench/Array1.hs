module ParkBench.Array1
  ( Array1,
    singleton,
    fromList,
    toList1,
    get,
  )
where

import Data.Array (Array)
import qualified Data.Array as Array
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.List.NonEmpty as List1
import ParkBench.Prelude

newtype Array1 a
  = Array1 (Array Int a)
  deriving stock (Foldable, Functor, Traversable)

singleton :: a -> Array1 a
singleton x =
  Array1 (Array.listArray (0, 1) [x])

fromList :: List.NonEmpty a -> Array1 a
fromList xs =
  Array1 (Array.listArray (0, Foldable.length xs - 1) (Foldable.toList xs))

toList1 :: Array1 a -> List.NonEmpty a
toList1 (Array1 xs) =
  List1.fromList (Foldable.toList xs)

get :: Int -> Array1 a -> a
get i (Array1 xs) =
  xs Array.! i
