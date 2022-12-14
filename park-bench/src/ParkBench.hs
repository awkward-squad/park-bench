module ParkBench
  ( Benchmark,
    benchmark,
    function,
    action,
  )
where

import Control.Concurrent (threadDelay)
import qualified Data.List.NonEmpty as List1
import qualified Data.Text as Text (pack)
import ParkBench.Internal.Array1 (Array1)
import qualified ParkBench.Internal.Array1 as Array1
import ParkBench.Internal.Benchable (Benchable)
import qualified ParkBench.Internal.Benchable as Benchable
import qualified ParkBench.Internal.Config as Config
import qualified ParkBench.Internal.Driver as Driver
import qualified ParkBench.Internal.Measure as Measure
import ParkBench.Internal.Named (Named (Named))
import qualified ParkBench.Internal.Named as Named
import ParkBench.Internal.Prelude
import ParkBench.Internal.Pretty (renderTable)
import ParkBench.Internal.Render (estimatesToTable)
import ParkBench.Internal.RtsStats (RtsStats)
import ParkBench.Internal.Statistics (Estimate)
import ParkBench.Internal.Terminal (renderToTerminal, withTerminal)

-- | A single benchmark.
newtype Benchmark
  = Benchmark (Named (Benchable ()))

-- | Run a collection of benchmarks.
benchmark ::
  -- | ⠀
  [Benchmark] ->
  IO void
benchmark benchmarks =
  case List1.nonEmpty benchmarks of
    Nothing -> forever (threadDelay maxBound)
    Just (benchable List1.:| []) -> benchmarkOne (coerce benchable)
    Just benchables -> benchmarkMany (Array1.fromList1 (coerce benchables))

benchmarkOne :: Named (Benchable ()) -> IO void
benchmarkOne benchable = do
  config <- Config.getFromEnv
  withRenderEstimatesToTerminal \renderEstimatesToTerminal -> do
    firstLiveBenchmark <-
      Driver.benchmark1 (Config.runlen config) (Benchable.mapIO Measure.measure (Named.thing benchable))
    loopForever firstLiveBenchmark \liveBenchmark -> do
      let estimates :: Array1 (Named (Estimate RtsStats))
          estimates = Array1.singleton (benchable $> Driver.sampleLiveBenchmark1 liveBenchmark)
      renderEstimatesToTerminal estimates
      Driver.stepLiveBenchmark1 liveBenchmark

benchmarkMany :: Array1 (Named (Benchable ())) -> IO void
benchmarkMany benchables = do
  config <- Config.getFromEnv
  withRenderEstimatesToTerminal \renderEstimatesToTerminal -> do
    estimates0 <-
      (traverse . traverse)
        (\benchable -> Driver.benchmark (Config.runlen config) (Benchable.mapIO Measure.measure benchable))
        benchables
    loopForever (Driver.makeLiveBenchmarks (Named.thing <$> estimates0)) \liveBenchmarks -> do
      estimates <- traverse (traverse Driver.sampleLiveBenchmark) estimates0
      renderEstimatesToTerminal estimates
      Driver.stepLiveBenchmarks liveBenchmarks

withRenderEstimatesToTerminal :: ((Array1 (Named (Estimate RtsStats)) -> IO ()) -> IO a) -> IO a
withRenderEstimatesToTerminal f =
  withTerminal \terminal ->
    f (renderToTerminal terminal . renderTable . estimatesToTable)

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
  Benchmark (Named (Text.pack name) (Benchable.function f x))

-- | Benchmark an IO action. The result is evaluated to weak head normal form.
action ::
  -- | ⠀
  String ->
  -- | ⠀
  IO a ->
  Benchmark
action name x =
  Benchmark (Named (Text.pack name) (Benchable.action x))
