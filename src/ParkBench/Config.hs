module ParkBench.Config
  ( Config (..),
    getFromEnv,
  )
where

import ParkBench.Prelude

data Config = Config
  { runlen :: Rational
  }

getFromEnv :: IO Config
getFromEnv = do
  pure
    Config
      { runlen = 100_000_000
      }
