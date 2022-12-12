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
import qualified Data.Text as Text (pack)
import ParkBench.Benchable (Benchable)
import qualified ParkBench.Benchable as Benchable
import qualified ParkBench.Driver as Driver
import qualified ParkBench.Measure as Measure
import ParkBench.Named (Named (Named))
import qualified ParkBench.Named as Named
import ParkBench.Prelude
import ParkBench.Pretty (renderTable)
import ParkBench.Render (estimatesToTable)
import ParkBench.RtsStats (RtsStats)
import ParkBench.Statistics (Estimate)
import ParkBench.Terminal (renderToTerminal, withTerminal)

-- | A single benchmark.
newtype Benchmark
  = Benchmark (Named (Benchable ()))

-- | Run a collection of benchmarks.
benchmark ::
  -- | ⠀
  [Benchmark] ->
  IO void
benchmark benchmarks =
  case NonEmpty.nonEmpty benchmarks of
    Nothing -> forever (threadDelay maxBound)
    Just (benchable :| []) -> benchmarkOne (coerce benchable)
    Just benchables -> benchmarkMany (coerce benchables)

benchmarkOne :: Named (Benchable ()) -> IO void
benchmarkOne benchable =
  withTerminal \terminal -> do
    loopForever (Driver.benchmark1 100_000_000 (Benchable.mapIO Measure.measure (Named.thing benchable))) \pull0 -> do
      (estimate, pull1) <- Driver.pull1 pull0
      let estimates :: NonEmpty (Named (Estimate RtsStats))
          estimates = (benchable $> estimate) :| []
      renderToTerminal terminal (renderTable (estimatesToTable estimates))
      pure pull1

benchmarkMany :: NonEmpty (Named (Benchable ())) -> IO void
benchmarkMany benchables =
  withTerminal \terminal -> do
    summaries0 <-
      (traverse . traverse)
        (\benchable -> Driver.benchmark 100_000_000 (Benchable.mapIO Measure.measure benchable))
        benchables
    loopForever (Driver.pulls (snd . Named.thing <$> summaries0)) \pulls0 -> do
      summaries <- traverse (traverse fst) summaries0
      renderToTerminal terminal (renderTable (estimatesToTable summaries))
      Driver.pull pulls0

loopForever :: forall a void. a -> (a -> IO a) -> IO void
loopForever x0 once =
  let loop :: a -> IO void
      loop x = do
        y <- once x
        loop y
   in loop x0

-- | Benchmark a function. The result is evaluated to weak head normal form.
function ::
  -- | ⠀
  String ->
  -- | ⠀
  (a -> b) ->
  -- | ⠀
  a ->
  Benchmark
function name f x =
  Benchmark (Named (Text.pack name) (Benchable.whnf f x))

-- | Benchmark an IO action. The result is evaluated to weak head normal form.
action ::
  -- | ⠀
  String ->
  -- | ⠀
  IO a ->
  Benchmark
action name x =
  Benchmark (Named (Text.pack name) (Benchable.whnfIO x))
