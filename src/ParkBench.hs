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
import qualified Data.Text.Encoding as Text
import qualified ParkBench.Benchmark as Benchmark
import qualified ParkBench.Builder as Builder
import ParkBench.Named (Named (Named))
import qualified ParkBench.Named as Named
import ParkBench.Prelude
import ParkBench.Pretty (renderTable)
import ParkBench.Render (estimatesToTable)
import ParkBench.RtsStats (RtsStats)
import qualified ParkBench.Statistics as Statistics
import ParkBench.Terminal (clearFromCursor, cursorUp, withTerminal)

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
benchmark' xs =
  withTerminal do
    summaries0 <- (traverse . traverse) Statistics.benchmark xs
    let pulls :: NonEmpty (Statistics.Pull RtsStats)
        pulls =
          snd . Named.thing <$> summaries0
    let getSummaries :: IO (NonEmpty (Named (Statistics.Estimate RtsStats)))
        getSummaries =
          traverse (traverse fst) summaries0
    let renderSummaries :: NonEmpty (Named (Statistics.Estimate RtsStats)) -> Int -> IO Int
        renderSummaries summaries newlines0 = do
          ByteString.putStr bytes
          pure (ByteString.count 10 bytes)
          where
            bytes =
              Text.encodeUtf8
                ( Builder.build
                    ( cursorUp newlines0
                        <> clearFromCursor
                        <> renderTable (estimatesToTable summaries)
                    )
                )
    let loop :: NonEmpty (Statistics.Pull RtsStats) -> Int -> IO void
        loop ps0 newlines0 = do
          summaries <- getSummaries
          newlines1 <- renderSummaries summaries newlines0
          ps1 <- Statistics.pull ps0
          loop ps1 newlines1
    ByteString.putStr (ByteString.singleton 10)
    loop pulls 0

-- | Benchmark a function. The result is evaluated to weak head normal form.
function ::
  -- |
  Text ->
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
  Text ->
  -- |
  IO a ->
  Benchmark
action name x =
  Benchmark (Named name (Benchmark.action x))
