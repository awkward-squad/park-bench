module ParkBench
  ( Benchmark,
    benchmark,
    function,
    action,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified ParkBench.InProcess as InProcess
import ParkBench.Prelude
import ParkBench.Pretty (renderTable)
import qualified ParkBench.Statistics as Statistics

newtype Benchmark
  = Benchmark (String, Word64 -> IO (Statistics.Timed InProcess.Summary))

benchmark :: [Benchmark] -> IO ()
benchmark xs =
  case NonEmpty.nonEmpty xs of
    Nothing -> pure ()
    Just ys -> benchmark' (coerce ys)

benchmark' :: NonEmpty (String, Word64 -> IO (Statistics.Timed InProcess.Summary)) -> IO void
benchmark' xs = do
  summaries0 <- (traverse . _2) Statistics.benchmark xs
  let loop :: NonEmpty (Statistics.Pull InProcess.Summary) -> IO void
      loop (Statistics.Pull _ p0 :| ps) = do
        summaries <- traverse (\(x, (y, _)) -> (x,) <$> y) summaries0
        putStrLn ("\ESC[2J" ++ renderTable (InProcess.summariesToTable summaries))
        p1 <- p0
        loop (Statistics.insertPull p1 ps)
  loop ((\(_, (_, x)) -> x) <$> summaries0)

function :: String -> (a -> b) -> a -> Benchmark
function name f x =
  Benchmark (name, InProcess.function (const f) x)

action :: String -> IO a -> Benchmark
action name x =
  Benchmark (name, InProcess.action x)

_2 :: Functor f => (b -> f c) -> (a, b) -> f (a, c)
_2 f (a, b) =
  (a,) <$> f b
