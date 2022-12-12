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
    let loop :: Driver.Pull1 RtsStats -> IO void
        loop pull0 = do
          (estimate, pull1) <- Driver.pull1 pull0
          renderToTerminal terminal (renderTable (estimatesToTable ((benchable $> estimate) :| [])))
          loop pull1
    loop (Driver.benchmark1 (Benchable.mapIO Measure.measure (Named.thing benchable)))

benchmarkMany :: NonEmpty (Named (Benchable ())) -> IO void
benchmarkMany benchables =
  withTerminal \terminal -> do
    summaries0 <-
      (traverse . traverse)
        (\benchable -> Driver.benchmark (Benchable.mapIO Measure.measure benchable))
        benchables
    let loop :: Driver.Pulls RtsStats -> IO void
        loop pulls0 = do
          summaries <- traverse (traverse fst) summaries0
          renderToTerminal terminal (renderTable (estimatesToTable summaries))
          pulls1 <- Driver.pull pulls0
          loop pulls1
    loop (Driver.pulls (snd . Named.thing <$> summaries0))

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
