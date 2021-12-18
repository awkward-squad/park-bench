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
import qualified ParkBench.Benchmark as Benchmark
import qualified ParkBench.Builder as Builder
import qualified ParkBench.Driver as Driver
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
  = Benchmark (Named (Word64 -> IO ()))

-- | Run a collection of benchmarks.
benchmark ::
  -- |
  [Benchmark] ->
  IO void
benchmark xs =
  case NonEmpty.nonEmpty xs of
    Nothing -> forever (threadDelay maxBound)
    Just ys -> benchmark' (coerce ys)

benchmark' :: NonEmpty (Named (Word64 -> IO ())) -> IO void
benchmark' = \case
  Named name f :| [] ->
    withTerminal do
      let loop :: Driver.Pull1 RtsStats -> Int -> IO void
          loop (Driver.Pull1 pull0) newlines0 = do
            (estimate, pull1) <- pull0
            newlines1 <- renderSummaries (Named name estimate :| []) newlines0
            loop pull1 newlines1
      ByteString.putStr (ByteString.singleton newline)
      loop (Driver.benchmark1 (Benchmark.measure . f)) 0
  fs ->
    withTerminal do
      summaries0 <- (traverse . traverse) (\f -> Driver.benchmark (Benchmark.measure . f)) fs
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
  -- |
  String ->
  -- |
  (a -> b) ->
  -- |
  a ->
  Benchmark
function name f x =
  Benchmark (Named (Text.pack name) (Benchmark.whnf f x))

-- | Benchmark an IO action. The result is evaluated to weak head normal form.
action ::
  -- |
  String ->
  -- |
  IO a ->
  Benchmark
action name x =
  Benchmark (Named (Text.pack name) (Benchmark.whnfIO x))
