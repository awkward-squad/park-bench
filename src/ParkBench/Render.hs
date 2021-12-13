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

estimatesToHeader :: NonEmpty (Named (Estimate RtsStats)) -> [Text]
estimatesToHeader (NonEmpty.toList -> names) =
  (if length names > 2 then (++ ["Total"]) else id) (go names)
  where
    go :: [Named (Estimate RtsStats)] -> [Text]
    go = \case
      [] -> []
      x : xs ->
        "" : (Text.map dash (Named.name x) <> "─(n=" <> Text.pack (show (samples (Named.thing x))) <> ")") : go xs

    dash :: Char -> Char
    dash = \case
      ' ' -> '─'
      c -> c

estimatesToRowGroups :: NonEmpty (Estimate RtsStats) -> [RowGroup]
estimatesToRowGroups (summary0 :| summaries0) =
  [ RowGroup
      "Elapsed time"
      [ render
          ( R
              "Total"
              (\e -> Just (EstSecondsCell (nanoseconds (mean e) / 1_000_000_000) (stdev e / 1_000_000_000)))
          ),
        render (R "Mutator" (Just . SecondsCell . (/ 1_000_000_000) . mutator_elapsed_ns . evalue)),
        render (R "Mutator %" (Just . PercentageCell' . mut_wall_percent . evalue)),
        render (R "Garbage collector" (Just . SecondsCell . (/ 1_000_000_000) . gc_elapsed_ns . evalue)),
        render (R "Garbage collector %" (Just . PercentageCell . gc_wall_percent . evalue))
      ],
    RowGroup
      "CPU time"
      [ render (R "Total" (Just . SecondsCell . (/ 1_000_000_000) . cpu_ns . evalue)),
        render (R "Mutator" (Just . SecondsCell . (/ 1_000_000_000) . mutator_cpu_ns . evalue)),
        render (R "Mutator %" (Just . PercentageCell' . mut_cpu_percent . evalue)),
        render (R "Garbage collector" (Just . SecondsCell . (/ 1_000_000_000) . gc_cpu_ns . evalue)),
        render (R "Garbage collector %" (Just . PercentageCell . gc_cpu_percent . evalue))
      ],
    RowGroup
      "Memory"
      [ render (R "Average residency" (Just . BytesCell . average_live_data . evalue)),
        render (R "Max residency (total)" (Just . BytesCell . max_live_bytes . evalue)),
        render (R "Max residency (normal objects)" (Just . BytesCell . max_normal_objects_bytes . evalue)),
        render (R "Max residency (large objects)" (Just . BytesCell . max_large_objects_bytes . evalue)),
        render (R "Max residency (compact regions)" (Just . BytesCell . max_compact_bytes . evalue)),
        render (R "Allocated" (Just . BytesCell . allocated_bytes . evalue)),
        render (R "Allocated per second" (Just . BytesPerSecondCell . allocated_bytes_per_second . evalue)),
        render (R "Copied during GC" (Just . BytesCell . copied_bytes . evalue)),
        render (R "Copied during parallel GC" (Just . BytesCell . par_copied_bytes . evalue)),
        render (R "Allocated from OS" (Just . BytesCell . max_mem_in_use_bytes . evalue)),
        render (R "Wasted by GHC" (Just . BytesCell . max_slop_bytes . evalue))
      ],
    -- TODO nonmoving GC
    RowGroup
      "Garbage collection"
      [ render (R "Collections (total)" (Just . NumberCell . gcs . evalue)),
        render (R "Collections (minor)" (Just . NumberCell . minor_gcs . evalue)),
        render (R "Collections (major)" (Just . NumberCell . major_gcs . evalue)),
        render (R "Total elapsed time" (Just . SecondsCell . (/ 1_000_000_000) . gc_elapsed_ns . evalue)),
        render (R "Total CPU time" (Just . SecondsCell . (/ 1_000_000_000) . gc_cpu_ns . evalue)),
        render (R "Average elapsed time" (Just . SecondsCell . (/ 1_000_000_000) . gc_average_ns . evalue)),
        render (R "Parallel work balance" (fmap PercentageCell' . work_balance . evalue))
      ]
  ]
  where
    evalue :: Estimate a -> a
    evalue =
      value . mean
    render :: forall a. Cellular a => R (Estimate RtsStats) a -> Row
    render =
      maketh (summary0 :| summaries0)
