module ParkBench
  ( Benchmark,
    benchmark,
    function,
    action,
  )
where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text (pack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import ParkBench.Benchable (Benchable)
import qualified ParkBench.Benchable as Benchable
import qualified ParkBench.Builder as Builder
import qualified ParkBench.Driver as Driver
import qualified ParkBench.Measure as Measure
import ParkBench.Named (Named (Named))
import qualified ParkBench.Named as Named
import ParkBench.Prelude
import ParkBench.Pretty (renderTable)
import ParkBench.Render (estimatesToTable)
import ParkBench.RtsStats (RtsStats)
import qualified ParkBench.Statistics as Statistics
import ParkBench.Terminal (clearFromCursor, cursorUp, withTerminal)

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
  withTerminal do
    let loop :: Driver.Pull1 RtsStats -> Int -> IO void
        loop (Driver.Pull1 pull0) newlines0 = do
          (estimate, pull1) <- pull0
          newlines1 <- renderSummaries ((benchable $> estimate) :| []) newlines0
          loop pull1 newlines1
    ByteString.putStr (ByteString.singleton newline)
    loop (Driver.benchmark1 (Benchable.mapIO Measure.measure (Named.thing benchable))) 0

benchmarkMany :: NonEmpty (Named (Benchable ())) -> IO void
benchmarkMany benchables =
  withTerminal do
    summaries0 <-
      (traverse . traverse)
        (\benchable -> Driver.benchmark (Benchable.mapIO Measure.measure benchable))
        benchables
    let loop :: Driver.Pulls RtsStats -> Int -> IO void
        loop pulls0 newlines0 = do
          summaries <- traverse (traverse fst) summaries0
          newlines1 <- renderSummaries summaries newlines0
          pulls1 <- Driver.pull pulls0
          loop pulls1 newlines1
    ByteString.putStr (ByteString.singleton newline)
    loop (Driver.pulls (snd . Named.thing <$> summaries0)) 0

renderSummaries :: NonEmpty (Named (Statistics.Estimate RtsStats)) -> Int -> IO Int
renderSummaries summaries newlines0 = do
  ByteString.putStr bytes
  pure (ByteString.count newline bytes)
  where
    bytes = Text.encodeUtf8 (Builder.build builder)
    builder = cursorUp newlines0 <> clearFromCursor <> renderTable (estimatesToTable summaries)

newline :: Word8
newline = 10

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
