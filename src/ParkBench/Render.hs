-- | Rendering code that takes lays out raw benchmarking results in a table.
module ParkBench.Render
  ( estimatesToTable,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import ParkBench.Named (Named)
import qualified ParkBench.Named as Named
import ParkBench.Prelude
import ParkBench.Pretty
import ParkBench.RtsStats
import ParkBench.Statistics

estimatesToTable :: NonEmpty (Named (Estimate RtsStats)) -> Table
estimatesToTable summaries =
  Table (estimatesToHeader summaries) (estimatesToRowGroups (Named.thing <$> summaries))

estimatesToHeader :: NonEmpty (Named (Estimate RtsStats)) -> [Cell]
estimatesToHeader (NonEmpty.toList -> names) =
  (if length names > 2 then (++ ["Total"]) else id) (go names)
  where
    go :: [Named (Estimate RtsStats)] -> [Cell]
    go = \case
      [] -> []
      x : xs -> EmptyCell : Cell Blue (Text.map dash (Named.name x)) : go xs

    dash :: Char -> Char
    dash = \case
      ' ' -> '─'
      c -> c

estimatesToRowGroups :: NonEmpty (Estimate RtsStats) -> [RowGroup]
estimatesToRowGroups (summary0 :| summaries0) =
  [ RowGroup
      "Statistics"
      [ render (R "Samples" (Just . IncomparableWord3Cell . samples)),
        render (R "CV (σ/μ)" (Just . IncomparablePercentageCell . goodness))
      ],
    RowGroup
      "Elapsed time"
      [ render (R "Total" (Just . NanosecondsCell . r2d . nanoseconds . mean)),
        render (R "Mutator" (Just . NanosecondsCell . r2d . mutator_elapsed_ns . value . mean)),
        render (R "Mutator %" (Just . PercentageCell' . r2d . mut_wall_percent . value . mean)),
        render (R "Garbage collector" (Just . NanosecondsCell . r2d . gc_elapsed_ns . value . mean)),
        render (R "Garbage collector %" (Just . PercentageCell . r2d . gc_wall_percent . value . mean))
      ],
    RowGroup
      "CPU time"
      [ render (R "Total" (Just . NanosecondsCell . r2d . cpu_ns . value . mean)),
        render (R "Mutator" (Just . NanosecondsCell . r2d . mutator_cpu_ns . value . mean)),
        render (R "Mutator %" (Just . PercentageCell' . r2d . mut_cpu_percent . value . mean)),
        render (R "Garbage collector" (Just . NanosecondsCell . r2d . gc_cpu_ns . value . mean)),
        render (R "Garbage collector %" (Just . PercentageCell . r2d . gc_cpu_percent . value . mean))
      ],
    RowGroup
      "Memory usage"
      [ render (R "Average" (Just . BytesCell . r2d . average_live_data . value . mean)),
        render (R "Maximum" (Just . BytesCell . r2d . max_live_bytes . value . mean))
      ],
    RowGroup
      "Memory pressure"
      [ render (R "Allocated" (Just . BytesCell . r2d . allocated_bytes . value . mean)),
        render (R "Allocated/sec" (Just . BytesPerSecondCell . r2d . allocated_bytes_per_second . value . mean)),
        render (R "Copied during GC" (Just . BytesCell . r2d . copied_bytes . value . mean))
      ],
    -- TODO nonmoving GC
    RowGroup
      "Garbage collection"
      [ render (R "Total collections" (Just . NumberCell . r2d . gcs . value . mean)),
        render (R "Major collections" (Just . NumberCell . r2d . major_gcs . value . mean)),
        render (R "Average pause" (Just . NanosecondsCell . r2d . gc_average_ns . value . mean)),
        render (R "Work balance" (fmap (PercentageCell' . r2d) . work_balance . value . mean))
      ]
  ]
  where
    render :: forall a. Cellular a => R (Estimate RtsStats) a -> Row
    render =
      rowMaker (summary0 :| summaries0)
