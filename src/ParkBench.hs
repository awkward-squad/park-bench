module ParkBench
  ( Benchmark,
    benchmark,
    function,
    action,
  )
where

import Control.Concurrent (threadDelay)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified ParkBench.Benchmark as Benchmark
import ParkBench.Named (Named (Named))
import qualified ParkBench.Named as Named
import ParkBench.Prelude
import ParkBench.Pretty (renderTable)
import ParkBench.Render (estimatesToTable)
import ParkBench.RtsStats (RtsStats)
import qualified ParkBench.Statistics as Statistics

-- | A benchmark.
newtype Benchmark
  = Benchmark (Named (Word64 -> IO (Statistics.Timed RtsStats)))

-- | Run a collection of benchmarks.
benchmark ::
  -- |
  [Benchmark] ->
  IO void
benchmark xs =
  case NonEmpty.nonEmpty xs of
    Nothing -> forever (threadDelay maxBound)
    Just ys -> benchmark' (coerce ys)

benchmark' :: NonEmpty (Named (Word64 -> IO (Statistics.Timed RtsStats))) -> IO void
benchmark' xs = do
  summaries0 <- (traverse . traverse) Statistics.benchmark xs
  let pulls :: NonEmpty (Statistics.Pull RtsStats)
      pulls =
        snd . Named.thing <$> summaries0
  let getSummaries :: IO (NonEmpty (Named (Statistics.Estimate RtsStats)))
      getSummaries =
        traverse (traverse fst) summaries0
  let renderSummaries :: IO ()
      renderSummaries = do
        summaries <- getSummaries
        putStrLn ("\ESC[2J" ++ renderTable (estimatesToTable summaries))
  let loop :: NonEmpty (Statistics.Pull RtsStats) -> IO void
      loop ps0 = do
        renderSummaries
        ps1 <- Statistics.pull ps0
        loop ps1
  loop pulls

-- | Benchmark a function. The result is evaluated to weak head normal form.
function ::
  -- |
  String ->
  -- |
  (a -> b) ->
  -- |
  a ->
  Benchmark
function name f x =
  Benchmark (Named name (Benchmark.function (const f) x))

-- | Benchmark an IO action. The result is evaluated to weak head normal form.
action ::
  -- |
  String ->
  -- |
  IO a ->
  Benchmark
action name x =
  Benchmark (Named name (Benchmark.action x))
