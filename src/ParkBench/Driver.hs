module ParkBench.Driver
  ( benchmark,
    Pull,
    Pulls,
    pulls,
    pull,
  )
where

import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import ParkBench.Prelude
import ParkBench.RtsStats (RtsStats)
import ParkBench.Statistics

benchmark :: forall a. Roll a => (Word64 -> IO (Timed a)) -> IO (IO (Estimate a), Pull a)
benchmark run = do
  t <- run 1
  let e = initialEstimate t
  ref <- newIORef e

  let another :: Estimate a -> IO (Pull a)
      another e0 = do
        t2 <- run n
        let !e1 = updateEstimate n t2 e0
        writeIORef ref e1
        pure (Pull (w2r (samples e1) * nanoseconds (mean e1)) (another e1))
        where
          n = next e0

  pure (readIORef ref, Pull (nanoseconds (mean e)) (another e))
  where
    -- target runs that take 0.1 seconds (e.g. 500_000_000 would be 0.5 seconds)
    next :: Estimate a -> Word64
    next Estimate {mean = Timed nanoseconds _, samples} =
      max 1 (min samples (floor (100_000_000 / nanoseconds)))
{-# SPECIALIZE benchmark :: (Word64 -> IO (Timed RtsStats)) -> IO (IO (Estimate RtsStats), Pull RtsStats) #-}

data Pull a
  = Pull
      -- amount of time this pull has gotten
      {-# UNPACK #-} !Rational
      !(IO (Pull a))

isMoreUrgentThan :: Pull a -> Pull a -> Bool
Pull t0 _ `isMoreUrgentThan` Pull t1 _ =
  t0 < t1

-- | An array of 'Pull', one per benchmark, in decreasing urgency order.
data Pulls a
  = P1 !(Pull a)
  | P2 !(Pull a) !(Pull a)
  | P3 !(Pull a) !(Pull a) !(Pull a)
  | Pn_ ![Pull a] -- invariant: 4+ elements

pattern Pn :: Pull a -> [Pull a] -> Pulls a
pattern Pn p ps <- Pn_ (p : ps)

{-# COMPLETE P1, P2, P3, Pn #-}

-- | Construct a 'Pulls' from a non-empty list of 'Pull'.
pulls :: NonEmpty (Pull a) -> Pulls a
pulls =
  pulls' . NonEmpty.sortWith \(Pull t _) -> t

pulls' :: NonEmpty (Pull a) -> Pulls a
pulls' = \case
  a :| [] -> P1 a
  a :| [b] -> P2 a b
  a :| [b, c] -> P3 a b c
  a :| as -> Pn_ (a : as)

pull :: Pulls a -> IO (Pulls a)
pull = \case
  P1 (Pull _ p0) -> do
    p <- p0
    pure (P1 p)
  P2 (Pull _ p0) q -> do
    p <- p0
    pure
      if q `isMoreUrgentThan` p
        then P2 q p
        else P2 p q
  P3 (Pull _ p0) q r -> do
    p <- p0
    pure
      if q `isMoreUrgentThan` p
        then
          if r `isMoreUrgentThan` p
            then P3 q r p
            else P3 q p r
        else P3 p q r
  Pn (Pull _ p0) ps -> do
    p <- p0
    pure (Pn_ (insertPull p ps))

insertPull :: Pull a -> [Pull a] -> [Pull a]
insertPull p0 = \case
  [] -> [p0]
  p1 : ps ->
    if p0 `isMoreUrgentThan` p1
      then p0 : p1 : ps
      else p1 : insertPull p0 ps
