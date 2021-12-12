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
  let pulls :: NonEmpty (Statistics.Pull InProcess.Summary)
      pulls =
        (\(_, (_, x)) -> x) <$> summaries0
  let getSummaries :: IO (NonEmpty (String, Statistics.Estimate InProcess.Summary))
      getSummaries =
        traverse (\(x, (y, _)) -> (x,) <$> y) summaries0
  let renderSummaries :: IO ()
      renderSummaries = do
        summaries <- getSummaries
        putStrLn ("\ESC[2J" ++ renderTable (InProcess.summariesToTable summaries))
  let loop :: NonEmpty (Statistics.Pull InProcess.Summary) -> IO void
      loop ps0 = do
        renderSummaries
        ps1 <- Statistics.pull ps0
        loop ps1
  loop pulls

function :: String -> (a -> b) -> a -> Benchmark
function name f x =
  Benchmark (name, InProcess.function (const f) x)

action :: String -> IO a -> Benchmark
action name x =
  Benchmark (name, InProcess.action x)

_2 :: Functor f => (b -> f c) -> (a, b) -> f (a, c)
_2 f (a, b) =
  (a,) <$> f b
