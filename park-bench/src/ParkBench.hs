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
import ParkBench.Array1 (Array1)
import qualified ParkBench.Array1 as Array1
import ParkBench.Benchable (Benchable)
import qualified ParkBench.Benchable as Benchable
import qualified ParkBench.Config as Config
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
  case List1.nonEmpty benchmarks of
    Nothing -> forever (threadDelay maxBound)
    Just (benchable List1.:| []) -> benchmarkOne (coerce benchable)
    Just benchables -> benchmarkMany (Array1.fromList (coerce benchables))

benchmarkOne :: Named (Benchable ()) -> IO void
benchmarkOne benchable = do
  config <- Config.getFromEnv
  withTerminal \terminal -> do
    let firstPull :: Driver.Pull1 RtsStats
        firstPull =
          Driver.benchmark1 (Config.runlen config) (Benchable.mapIO Measure.measure (Named.thing benchable))
    loopForever firstPull \pull0 -> do
      (estimate, pull1) <- Driver.pull1 pull0
      let estimates :: Array1 (Named (Estimate RtsStats))
          estimates = Array1.singleton (benchable $> estimate)
      renderToTerminal terminal (renderTable (estimatesToTable estimates))
      pure pull1

benchmarkMany :: Array1 (Named (Benchable ())) -> IO void
benchmarkMany benchables = do
  config <- Config.getFromEnv
  withTerminal \terminal -> do
    summaries0 <-
      (traverse . traverse)
        (\benchable -> Driver.benchmark (Config.runlen config) (Benchable.mapIO Measure.measure benchable))
        benchables
    loopForever (Driver.makePulls (snd . Named.thing <$> summaries0)) \pulls0 -> do
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
