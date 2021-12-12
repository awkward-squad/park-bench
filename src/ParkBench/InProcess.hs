-- | In-process benchmarking. TODO find a better name for the module
module ParkBench.InProcess
  ( function,
    action,
    Summary (..),
    summariesToTable,
  )
where

import Control.Exception (evaluate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.Stats as GHC
import ParkBench.Numeric (divide, divide')
import ParkBench.Prelude
import ParkBench.Pretty
import ParkBench.Statistics
import System.Mem (performGC)
import Text.Printf (printf)

timeit :: IO () -> IO (Timed Summary)
timeit act = do
  performGC
  s0 <- GHC.getRTSStats
  act
  -- Unfortunate that this throws off the timing stats a bit, but some memory stats are only updated on major GCs
  performGC
  s1 <- GHC.getRTSStats
  let measure :: Integral a => (GHC.RTSStats -> a) -> Rational
      measure f =
        fromIntegral (f s1 - f s0)
  let gauge :: Integral a => (GHC.RTSStats -> a) -> Rational
      gauge f =
        fromIntegral (max (f s1) (f s0))
  let elapsed_ns :: Rational
      elapsed_ns =
        measure GHC.elapsed_ns
  pure
    Timed
      { time = elapsed_ns,
        value =
          Summary
            { allocated_bytes = measure GHC.allocated_bytes,
              copied_bytes = measure GHC.copied_bytes,
              cpu_ns = measure GHC.cpu_ns,
              cumulative_live_bytes = measure GHC.cumulative_live_bytes,
              cumulative_par_balanced_copied_bytes = measure GHC.cumulative_par_balanced_copied_bytes,
              elapsed_ns,
              gc_cpu_ns = measure GHC.gc_cpu_ns,
              gc_elapsed_ns = measure GHC.gc_elapsed_ns,
              gcs = measure GHC.gcs,
              major_gcs = measure GHC.major_gcs,
              max_compact_bytes = gauge GHC.max_compact_bytes,
              max_large_objects_bytes = gauge GHC.max_large_objects_bytes,
              max_live_bytes = gauge GHC.max_live_bytes,
              max_mem_in_use_bytes = gauge GHC.max_mem_in_use_bytes,
              max_slop_bytes = gauge GHC.max_slop_bytes,
              mutator_cpu_ns = measure GHC.mutator_cpu_ns,
              mutator_elapsed_ns = measure GHC.mutator_elapsed_ns,
              par_copied_bytes = measure GHC.par_copied_bytes
            }
      }

function :: (forall r. r -> a -> b) -> a -> Word64 -> IO (Timed Summary)
function f x =
  timeit . go
  where
    go :: Word64 -> IO ()
    go = \case
      0 -> pure ()
      n -> do
        -- problem with `f x`: it could be let-floated
        _ <- evaluate (f n x)
        go (n -1)
{-# NOINLINE function #-}

action :: IO a -> Word64 -> IO (Timed Summary)
action x =
  timeit . go
  where
    go :: Word64 -> IO ()
    go = \case
      0 -> pure ()
      n -> do
        _ <- evaluate x
        go (n -1)

summariesToTable :: NonEmpty (String, Estimate Summary) -> Table
summariesToTable summaries =
  Table (summariesToTableHeader summaries) (summariesToTableRowGroups (snd <$> summaries))

summariesToTableHeader :: NonEmpty (String, Estimate Summary) -> [String]
summariesToTableHeader (NonEmpty.toList -> names) =
  (if length names > 2 then (++ ["Total"]) else id) (go names)
  where
    go :: [(String, Estimate Summary)] -> [String]
    go = \case
      [] -> []
      (x, e) : xs -> "" : printf "%s─(%.0f%%)" (map dash x) (goodness e * 100) : go xs

    dash :: Char -> Char
    dash = \case
      ' ' -> '─'
      c -> c

summariesToTableRowGroups :: NonEmpty (Estimate Summary) -> [RowGroup]
summariesToTableRowGroups (summary0 :| summaries0) =
  [ RowGroup
      "Elapsed time"
      [ render (R "Total" (\(Estimate (Timed m _) s) -> Just (EstSecondsCell (m / 1_000_000_000) (s / 1_000_000_000)))),
        render (R "Mutator" (Just . SecondsCell . (/ 1_000_000_000) . mutator_elapsed_ns . value . mean)),
        render (R "Mutator %" (Just . PercentageCell' . mut_wall_percent . value . mean)),
        render (R "Garbage collector" (Just . SecondsCell . (/ 1_000_000_000) . gc_elapsed_ns . value . mean)),
        render (R "Garbage collector %" (Just . PercentageCell . gc_wall_percent . value . mean))
      ],
    RowGroup
      "CPU time"
      [ render (R "Total" (Just . SecondsCell . (/ 1_000_000_000) . cpu_ns . value . mean)),
        render (R "Mutator" (Just . SecondsCell . (/ 1_000_000_000) . mutator_cpu_ns . value . mean)),
        render (R "Mutator %" (Just . PercentageCell' . mut_cpu_percent . value . mean)),
        render (R "Garbage collector" (Just . SecondsCell . (/ 1_000_000_000) . gc_cpu_ns . value . mean)),
        render (R "Garbage collector %" (Just . PercentageCell . gc_cpu_percent . value . mean))
      ],
    RowGroup
      "Memory"
      [ render (R "Average residency" (Just . BytesCell . average_live_data . value . mean)),
        render (R "Max residency (total)" (Just . BytesCell . max_live_bytes . value . mean)),
        render (R "Max residency (normal objects)" (Just . BytesCell . max_normal_objects_bytes . value . mean)),
        render (R "Max residency (large objects)" (Just . BytesCell . max_large_objects_bytes . value . mean)),
        render (R "Max residency (compact regions)" (Just . BytesCell . max_compact_bytes . value . mean)),
        render (R "Allocated" (Just . BytesCell . allocated_bytes . value . mean)),
        render (R "Allocated per second" (Just . BytesPerSecondCell . allocated_bytes_per_second . value . mean)),
        render (R "Copied during GC" (Just . BytesCell . copied_bytes . value . mean)),
        render (R "Copied during parallel GC" (Just . BytesCell . par_copied_bytes . value . mean)),
        render (R "Allocated from OS" (Just . BytesCell . max_mem_in_use_bytes . value . mean)),
        render (R "Wasted by GHC" (Just . BytesCell . max_slop_bytes . value . mean))
      ],
    -- TODO nonmoving GC
    RowGroup
      "Garbage collection"
      [ render (R "Collections (total)" (Just . NumberCell . gcs . value . mean)),
        render (R "Collections (minor)" (Just . NumberCell . minor_gcs . value . mean)),
        render (R "Collections (major)" (Just . NumberCell . major_gcs . value . mean)),
        render (R "Total elapsed time" (Just . SecondsCell . (/ 1_000_000_000) . gc_elapsed_ns . value . mean)),
        render (R "Total CPU time" (Just . SecondsCell . (/ 1_000_000_000) . gc_cpu_ns . value . mean)),
        render (R "Average elapsed time" (Just . SecondsCell . (/ 1_000_000_000) . gc_average_ns . value . mean)),
        render (R "Parallel work balance" (fmap PercentageCell' . work_balance . value . mean))
      ]
  ]
  where
    render :: forall a. Cellular a => R (Estimate Summary) a -> Row
    render =
      maketh (summary0 :| summaries0)

-- TODO nonmoving_gc_*
data Summary = Summary
  { allocated_bytes :: !Rational,
    copied_bytes :: !Rational,
    cpu_ns :: !Rational,
    cumulative_live_bytes :: !Rational,
    cumulative_par_balanced_copied_bytes :: !Rational,
    elapsed_ns :: !Rational,
    gc_cpu_ns :: !Rational,
    gc_elapsed_ns :: !Rational,
    gcs :: !Rational,
    major_gcs :: !Rational,
    max_compact_bytes :: !Rational,
    max_large_objects_bytes :: !Rational,
    max_live_bytes :: !Rational,
    max_mem_in_use_bytes :: !Rational,
    max_slop_bytes :: !Rational,
    mutator_cpu_ns :: !Rational,
    mutator_elapsed_ns :: !Rational,
    par_copied_bytes :: !Rational
  }
  deriving stock (Show)

instance Sample Summary where
  combine2 s0 s1 =
    Summary
      { allocated_bytes = measure allocated_bytes,
        copied_bytes = measure copied_bytes,
        cpu_ns = measure cpu_ns,
        cumulative_live_bytes = measure cumulative_live_bytes,
        cumulative_par_balanced_copied_bytes = measure cumulative_par_balanced_copied_bytes,
        elapsed_ns = measure elapsed_ns,
        gc_cpu_ns = measure gc_cpu_ns,
        gc_elapsed_ns = measure gc_elapsed_ns,
        gcs = measure gcs,
        major_gcs = measure major_gcs,
        max_compact_bytes = gauge max_compact_bytes,
        max_large_objects_bytes = gauge max_large_objects_bytes,
        max_live_bytes = gauge max_live_bytes,
        max_mem_in_use_bytes = gauge max_mem_in_use_bytes,
        max_slop_bytes = gauge max_slop_bytes,
        mutator_cpu_ns = measure mutator_cpu_ns,
        mutator_elapsed_ns = measure mutator_elapsed_ns,
        par_copied_bytes = measure par_copied_bytes
      }
    where
      measure :: (Summary -> Rational) -> Rational
      measure f =
        fit (f s0) (f s1)

      gauge :: (Summary -> Rational) -> Rational
      gauge f =
        max (f s0) (f s1)

instance Scaled Summary where
  downscale n s =
    Summary
      { allocated_bytes = measure allocated_bytes,
        copied_bytes = measure copied_bytes,
        cpu_ns = measure cpu_ns,
        cumulative_live_bytes = measure cumulative_live_bytes,
        cumulative_par_balanced_copied_bytes = measure cumulative_par_balanced_copied_bytes,
        elapsed_ns = measure elapsed_ns,
        gc_cpu_ns = measure gc_cpu_ns,
        gc_elapsed_ns = measure gc_elapsed_ns,
        gcs = measure gcs,
        major_gcs = measure major_gcs,
        max_compact_bytes = gauge max_compact_bytes,
        max_large_objects_bytes = gauge max_large_objects_bytes,
        max_live_bytes = gauge max_live_bytes,
        max_mem_in_use_bytes = gauge max_mem_in_use_bytes,
        max_slop_bytes = gauge max_slop_bytes,
        mutator_cpu_ns = measure mutator_cpu_ns,
        mutator_elapsed_ns = measure mutator_elapsed_ns,
        par_copied_bytes = measure par_copied_bytes
      }
    where
      measure :: Scaled a => (Summary -> a) -> a
      measure f =
        downscale n (f s)

      gauge :: (Summary -> a) -> a
      gauge f =
        f s

allocated_bytes_per_second :: Summary -> Rational
allocated_bytes_per_second s =
  allocated_bytes s `divide` (elapsed_ns s / 1_000_000_000)

average_live_data :: Summary -> Rational
average_live_data s =
  cumulative_live_bytes s `divide` major_gcs s

gc_average_ns :: Summary -> Rational
gc_average_ns s =
  gcs s `divide` gc_elapsed_ns s

gc_cpu_percent :: Summary -> Rational
gc_cpu_percent s =
  gc_cpu_ns s `divide` cpu_ns s

gc_wall_percent :: Summary -> Rational
gc_wall_percent s =
  gc_elapsed_ns s `divide` elapsed_ns s

max_normal_objects_bytes :: Summary -> Rational
max_normal_objects_bytes s =
  max_live_bytes s - max_compact_bytes s - max_large_objects_bytes s

minor_gcs :: Summary -> Rational
minor_gcs s =
  gcs s - major_gcs s

mut_cpu_percent :: Summary -> Rational
mut_cpu_percent s =
  mutator_cpu_ns s `divide` cpu_ns s

mut_wall_percent :: Summary -> Rational
mut_wall_percent s =
  mutator_elapsed_ns s `divide` elapsed_ns s

work_balance :: Summary -> Maybe Rational
work_balance s =
  cumulative_par_balanced_copied_bytes s `divide'` par_copied_bytes s
