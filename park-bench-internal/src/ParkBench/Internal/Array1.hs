module ParkBench.Internal.Array1
  ( Array1,
    singleton,
    fromList,
    toList1,
    get,
    uncons,
  )
where

import Data.Array (Array)
import qualified Data.Array as Array
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.List.NonEmpty as List1
import ParkBench.Internal.Prelude

data Array1 a
  = Array1 a (Array Int a)
  deriving stock (Foldable, Functor, Traversable)

singleton :: a -> Array1 a
singleton x =
  Array1 x (Array.array (1, 0) []) -- bogus bounds is the only way to make an empty array LOL

fromList :: List.NonEmpty a -> Array1 a
fromList (x List1.:| xs) =
  Array1 x (Array.listArray (0, Foldable.length xs - 1) xs)

toList1 :: Array1 a -> List.NonEmpty a
toList1 (Array1 x xs) =
  x List1.:| Foldable.toList xs

get :: Int -> Array1 a -> a
get i (Array1 x xs) =
  if i == 0 then x else xs Array.! (i - 1)

uncons :: Array1 a -> (a, Array Int a)
uncons (Array1 x xs) =
  (x, xs)
