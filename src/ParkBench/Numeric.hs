module ParkBench.Numeric
  ( divide,
    divide',
  )
where

import ParkBench.Prelude

divide :: Rational -> Rational -> Rational
divide n d =
  if d == 0 then 0 else n / d

divide' :: Rational -> Rational -> Maybe Rational
divide' n d =
  if d == 0 then Nothing else Just (n / d)
