-- | Generic-ish (not really) pretty-printing utilities and abstractions for rendering the table of cells.
module ParkBench.Pretty
  ( -- * High-level row/cell machinery
    R (..),
    Cellular (..),
    BytesCell (..),
    BytesPerSecondCell (..),
    EstNanosecondsCell (..),
    NanosecondsCell (..),
    NumberCell (..),
    NumberCell' (..),
    PercentageCell (..),
    PercentageCell' (..),
    rowMaker,

    -- *
    Table (..),
    renderTable,
    RowGroup (..),
    Row (..),
    Cell (..),
    isEmptyCell,
    Color (..),
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Ord (Down (..))
import Data.Ratio (approxRational)
import Data.String (IsString (..))
import qualified Data.Text as Text
import ParkBench.Builder (Builder)
import qualified ParkBench.Builder as Builder
import ParkBench.Prelude
import qualified ParkBench.Terminal as Terminal

------------------------------------------------------------------------------------------------------------------------
-- High-level row/cell machinery

data R a b
  = R Cell (a -> Maybe b)

class Ord a => Cellular a where
  cellString :: a -> Builder
  cellValue :: a -> Rational

newtype BytesCell
  = BytesCell Rational
  deriving (Eq) via Rational
  deriving (Ord) via Down Rational

instance Cellular BytesCell where
  cellString (BytesCell r) = Builder.bytes4 r
  cellValue = coerce

newtype BytesPerSecondCell
  = BytesPerSecondCell Rational
  deriving (Eq) via Rational
  deriving (Ord) via Down Rational

instance Cellular BytesPerSecondCell where
  cellString (BytesPerSecondCell r) = prettyBytesPerSecond r
  cellValue = coerce

prettyBytesPerSecond :: Rational -> Builder
prettyBytesPerSecond r =
  if Builder.null s
    then Builder.empty
    else s <> "/s"
  where
    s = Builder.bytes4 r

newtype NumberCell
  = NumberCell Rational
  deriving (Eq) via Rational
  deriving (Ord) via Down Rational

instance Cellular NumberCell where
  cellString (NumberCell r) = Builder.rational4 r
  cellValue = coerce

newtype NumberCell'
  = NumberCell' Rational
  deriving (Eq, Ord) via Rational
  deriving (Cellular) via NumberCell

data EstNanosecondsCell
  = EstNanosecondsCell
      -- mean
      {-# UNPACK #-} !Rational
      -- standard deviation
      {-# UNPACK #-} !Double

instance Eq EstNanosecondsCell where
  EstNanosecondsCell x _ == EstNanosecondsCell y _ = x == y

instance Ord EstNanosecondsCell where
  compare (EstNanosecondsCell x _) (EstNanosecondsCell y _) = compare y x

instance Cellular EstNanosecondsCell where
  cellString (EstNanosecondsCell x y) = prettyEstNanoseconds x y
  cellValue (EstNanosecondsCell x _) = x

prettyEstNanoseconds :: Rational -> Double -> Builder
prettyEstNanoseconds n m =
  if Builder.null m'
    then Builder.nanos4 n
    else Builder.nanos4 n <> " ±" <> m'
  where
    m' = Builder.nanos3 (2 * (approxRational m (1 / 1_000_000_000)))

newtype NanosecondsCell
  = NanosecondsCell Rational
  deriving (Eq) via Rational
  deriving (Ord) via Down Rational

instance Cellular NanosecondsCell where
  cellString (NanosecondsCell r) = Builder.nanos4 r
  cellValue = coerce

newtype PercentageCell
  = PercentageCell Rational
  deriving (Eq) via Rational
  deriving (Ord) via Down Rational

instance Cellular PercentageCell where
  cellString (PercentageCell r) = Builder.percentage r
  cellValue = coerce

prettyDelta :: Rational -> Builder
prettyDelta n
  | n >= 1 = Builder.multiplier (n + 1)
  | n <= -0.5 = Builder.multiplier (-1 `divide` (n + 1))
  | otherwise = Builder.percentage n

newtype PercentageCell'
  = PercentageCell' Rational
  deriving (Eq, Ord) via Rational
  deriving (Cellular) via PercentageCell

rowMaker :: forall a. NonEmpty a -> (forall b. Cellular b => R a b -> Row)
rowMaker (summary0 :| summaries0) (R name (f :: a -> Maybe b)) =
  if all isEmptyCell cols
    then EmptyRow
    else Row (name : cols)
  where
    cols :: [Cell]
    cols =
      maybe EmptyCell (Cell White . Builder.build . cellString) (f summary0) : makeCols (f summary0) summaries0
    -- TODO make this cleaner
    makeCols :: Maybe b -> [a] -> [Cell]
    makeCols s0 = \case
      [] ->
        case summaries0 of
          [] -> []
          _ ->
            case (f summary0, f (last summaries0)) of
              (Just v0, Just v1) -> [delta v0 v1]
              _ -> []
      s1 : ss ->
        case (s0, f s1) of
          (Nothing, Just v1) -> EmptyCell : Cell White (Builder.build (cellString v1)) : makeCols (Just v1) ss
          (Just v0, Just v1) -> delta v0 v1 : Cell White (Builder.build (cellString v1)) : makeCols (Just v1) ss
          (_, Nothing) -> EmptyCell : EmptyCell : makeCols Nothing ss
    delta :: b -> b -> Cell
    delta v1 v2 =
      if Builder.null (cellString v1) || Builder.null (cellString v2)
        then EmptyCell
        else colorize (prettyDelta ((cellValue v2 - cellValue v1) `divide` cellValue v1))
      where
        colorize :: Builder -> Cell
        colorize =
          ( case compare v1 v2 of
              LT -> Cell Green
              EQ -> Cell White
              GT -> Cell Red
          )
            . Builder.build

data Table
  = Table ![Cell] ![RowGroup]

data RowGroup
  = RowGroup {-# UNPACK #-} !Text ![Row]

data Row
  = -- | Invariant: 1+ cells; not all cells are empty
    Row ![Cell]
  | EmptyRow

data Cell
  = Append !Cell !Cell
  | EmptyCell
  | Cell !Color {-# UNPACK #-} !Text

cellBuilder :: Cell -> Builder
cellBuilder = \case
  Append x y -> cellBuilder x <> cellBuilder y
  EmptyCell -> Builder.empty
  Cell color (Builder.t -> s) ->
    case color of
      Blue -> Terminal.blue s
      Green -> Terminal.green s
      Red -> Terminal.red s
      White -> s

cellWidth :: Cell -> Int
cellWidth = \case
  Append x y -> cellWidth x + cellWidth y
  Cell _ s -> Text.length s
  EmptyCell -> 0

instance IsString Cell where
  fromString =
    Cell White . Text.pack

data Color
  = Blue
  | Green
  | Red
  | White

isEmptyCell :: Cell -> Bool
isEmptyCell = \case
  Append x y -> isEmptyCell x && isEmptyCell y
  -- don't yet have invariant that s is non-null
  Cell _ s -> Text.null s
  EmptyCell -> True

data Align
  = AlignLeft
  | AlignRight

renderTable :: Table -> Builder
renderTable (Table labels rowGroups) =
  (header : mapMaybe renderRowGroup rowGroups ++ [footer]) `Builder.sepBy` Builder.c '\n'
  where
    header :: Builder
    header =
      Builder.c '┌'
        <> ((map (renderCell AlignLeft '─') (zip (map (+ 2) widths) labels)) `Builder.sepBy` Builder.c '┬')
        <> Builder.c '┐'

    footer :: Builder
    footer =
      Builder.c '└'
        <> ((map (\n -> Builder.cs (n + 2) '─') widths) `Builder.sepBy` Builder.c '┴')
        <> Builder.c '┘'

    renderRowGroup :: RowGroup -> Maybe Builder
    renderRowGroup (RowGroup label rows) =
      case mapMaybe renderRow rows of
        [] -> Nothing
        s -> Just ((line : s) `Builder.sepBy` Builder.c '\n')
      where
        line =
          Builder.c '├'
            <> "\ESC[1m\ESC[97m"
            <> Builder.t (Text.map dash label)
            <> "\ESC[39m\ESC[22m"
            <> Builder.cs (head widths + 2 - Text.length label) '─'
            <> foldMap (\n -> Builder.c '┼' <> Builder.cs (n + 2) '─') (tail widths)
            <> Builder.c '┤'

        dash :: Char -> Char
        dash = \case
          ' ' -> '─'
          c -> c

    renderRow :: Row -> Maybe Builder
    renderRow = \case
      Row row -> Just ("│ " <> ((map (renderCell AlignRight ' ') (zip widths row)) `Builder.sepBy` " │ ") <> " │")
      EmptyRow -> Nothing

    renderCell :: Align -> Char -> (Int, Cell) -> Builder
    renderCell align bg (n, cell) =
      case align of
        AlignLeft -> cellBuilder cell <> padding
        AlignRight -> padding <> cellBuilder cell
      where
        padding = Builder.cs (n - cellWidth cell) bg

    widths :: [Int]
    widths =
      foldl' step0 (map (subtract 1 . cellWidth) labels) rowGroups
      where
        step0 :: [Int] -> RowGroup -> [Int]
        step0 acc (RowGroup label rows) =
          foldl' (step1 label) acc rows

        step1 :: Text -> [Int] -> Row -> [Int]
        step1 label acc = \case
          Row [] -> error "empty row"
          Row (col : cols) -> zipWith max (max (Text.length label) (cellWidth col) : map cellWidth cols) acc
          EmptyRow -> acc
