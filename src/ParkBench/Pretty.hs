module ParkBench.Pretty
  ( -- * High-level row/cell machinery
    R (..),
    Cellular (..),
    BytesCell (..),
    BytesPerSecondCell (..),
    EstSecondsCell (..),
    NumberCell (..),
    NumberCell' (..),
    PercentageCell (..),
    PercentageCell' (..),
    SecondsCell (..),
    maketh,

    -- *
    Table (..),
    RowGroup (..),
    Row (..),
    Cell (..),
    Color (..),
    isEmptyCell,
    green,
    red,
    white,
    renderTable,
  )
where

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Ord (Down (..))
import Data.Ratio (approxRational)
import Data.String (IsString (..))
import ParkBench.Numeric (divide)
import ParkBench.Prelude
import Text.Printf (printf)

------------------------------------------------------------------------------------------------------------------------
-- High-level row/cell machinery

data R a b
  = R Cell (a -> Maybe b)

class (Ord a, Show a) => Cellular a where
  cellValue :: a -> Rational

newtype BytesCell
  = BytesCell Rational
  deriving (Eq) via Rational
  deriving (Ord) via Down Rational

instance Cellular BytesCell where
  cellValue = coerce

instance Show BytesCell where
  show (BytesCell r) = prettyBytes r

prettyBytes :: Rational -> String
prettyBytes n0
  | n1 <- n0 / 1_000_000_000, n1 >= 1 = prettyNumber n1 ++ " gb"
  | n1 <- n0 / 1_000_000, n1 >= 1 = prettyNumber n1 ++ " mb"
  | n1 <- n0 / 1_000, n1 >= 1 = prettyNumber n1 ++ " kb"
  | n0 >= 0.5 = prettyNumber n0 ++ " b"
  | otherwise = ""

newtype BytesPerSecondCell
  = BytesPerSecondCell Rational
  deriving (Eq) via Rational
  deriving (Ord) via Down Rational

instance Cellular BytesPerSecondCell where
  cellValue = coerce

instance Show BytesPerSecondCell where
  show (BytesPerSecondCell r) = prettyBytesPerSecond r

prettyBytesPerSecond :: Rational -> String
prettyBytesPerSecond r =
  case prettyBytes r of
    "" -> ""
    s -> s ++ "/s"

newtype NumberCell
  = NumberCell Rational
  deriving (Eq) via Rational
  deriving (Ord) via Down Rational

instance Cellular NumberCell where
  cellValue = coerce

instance Show NumberCell where
  show (NumberCell r) = prettyNumber r

newtype NumberCell'
  = NumberCell' Rational
  deriving (Eq, Ord) via Rational
  deriving (Cellular, Show) via NumberCell

data EstSecondsCell
  = EstSecondsCell Rational Double

instance Eq EstSecondsCell where
  EstSecondsCell x _ == EstSecondsCell y _ = x == y

instance Ord EstSecondsCell where
  compare (EstSecondsCell x _) (EstSecondsCell y _) = compare y x

instance Cellular EstSecondsCell where
  cellValue (EstSecondsCell x _) = x

instance Show EstSecondsCell where
  show (EstSecondsCell x y) = prettyEstSeconds x y

prettyEstSeconds :: Rational -> Double -> String
prettyEstSeconds n m =
  case prettySeconds (approxRational m (1 / 1_000_000_000)) of
    "" -> prettySeconds n
    m' -> prettySeconds n ++ " ± " ++ m'

newtype PercentageCell
  = PercentageCell Rational
  deriving (Eq) via Rational
  deriving (Ord) via Down Rational

instance Cellular PercentageCell where
  cellValue = coerce

instance Show PercentageCell where
  show (PercentageCell r) = prettyPercentage r

prettyPercentage :: Rational -> String
prettyPercentage n =
  case prettyNumber (n * 100) of
    "" -> ""
    s -> s ++ "%"

newtype PercentageCell'
  = PercentageCell' Rational
  deriving (Eq, Ord) via Rational
  deriving (Cellular, Show) via PercentageCell

newtype SecondsCell
  = SecondsCell Rational
  deriving (Eq) via Rational
  deriving (Ord) via Down Rational

instance Cellular SecondsCell where
  cellValue = coerce

instance Show SecondsCell where
  show (SecondsCell r) = prettySeconds r

prettySeconds :: Rational -> String
prettySeconds n0
  | n0 >= 1 = prettyNumber n0 ++ " s"
  | n1 <- n0 * 1_000, n1 >= 1 = prettyNumber n1 ++ " ms"
  | n1 <- n0 * 1_000_000, n1 >= 1 = prettyNumber n1 ++ " µs"
  | otherwise =
    case prettyNumber (n0 * 1_000_000_000) of
      "" -> ""
      s -> s ++ " ns"

-- | Print a number with three significant digits.
prettyNumber :: Rational -> String
prettyNumber n
  | na >= 100 = printf "%.0f" nd
  | na >= 10 = printf "%.1f" nd
  | na >= 1 = printf "%.2f" nd
  | otherwise =
    case printf "%.3f" nd of
      "0.000" -> ""
      s -> s
  where
    na = abs n
    nd = realToFrac n :: Double

-- TODO rename
maketh :: forall a. NonEmpty a -> (forall b. Cellular b => R a b -> Row)
maketh (summary0 :| summaries0) (R name (f :: a -> Maybe b)) =
  if all isEmptyCell cols
    then Empty
    else Row (name : cols)
  where
    cols :: [Cell]
    cols =
      white (maybe "" show (f summary0)) : makeCols (f summary0) summaries0
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
          (Nothing, Just v1) -> "" : white (show v1) : makeCols (Just v1) ss
          (Just v0, Just v1) -> delta v0 v1 : white (show v1) : makeCols (Just v1) ss
          (_, Nothing) -> "" : "" : makeCols Nothing ss
    delta :: b -> b -> Cell
    delta v1 v2 =
      if null (show v1) || null (show v2)
        then ""
        else colorize (prettyPercentage ((cellValue v2 - cellValue v1) `divide` cellValue v1))
      where
        colorize :: String -> Cell
        colorize =
          case compare v1 v2 of
            LT -> green
            EQ -> white
            GT -> red

data Table
  = Table [String] [RowGroup]

data RowGroup
  = RowGroup String [Row]

data Row
  = -- | Invariant: 1+ cells; not all cells are empty
    Row [Cell]
  | Empty

data Cell
  = Cell Color String

instance IsString Cell where
  fromString =
    white

data Color
  = White
  | Red
  | Green

green :: String -> Cell
green = Cell Green

red :: String -> Cell
red = Cell Red

white :: String -> Cell
white = Cell White

isEmptyCell :: Cell -> Bool
isEmptyCell (Cell _ s) =
  null s

renderTable :: Table -> String
renderTable (Table labels rowGroups) =
  List.intercalate "\n" (header : mapMaybe renderRowGroup rowGroups ++ [footer])
  where
    header :: String
    header =
      let middle = List.intercalate "┬" (map (\(s, n) -> s ++ replicate (n + 2 - length s) '─') (zip labels widths))
       in '┌' : middle ++ "┐"
    line :: String -> String
    line label =
      let label' = "\ESC[1m\ESC[4m\ESC[97m" ++ label ++ "\ESC[39m\ESC[24m\ESC[22m"
          segment = label' ++ replicate (head widths + 2 - length label) '─'
          segments = concatMap (\n -> '┼' : replicate (n + 2) '─') (tail widths)
       in '├' : segment ++ segments ++ "┤"
    footer :: String
    footer =
      '└' : List.intercalate "┴" (map (\n -> replicate (n + 2) '─') widths) ++ "┘"
    renderRowGroup :: RowGroup -> Maybe String
    renderRowGroup (RowGroup label rows) =
      case mapMaybe renderRow rows of
        [] -> Nothing
        s -> Just (List.intercalate "\n" (line label : s))
    renderRow :: Row -> Maybe String
    renderRow = \case
      Row row -> Just ("│ " ++ List.intercalate " │ " (map renderCell (zip widths row)) ++ " │")
      Empty -> Nothing
    renderCell :: (Int, Cell) -> String
    renderCell (n, Cell color s) =
      case color of
        White -> s'
        Green -> "\ESC[32m" ++ s' ++ "\ESC[39m"
        Red -> "\ESC[31m" ++ s' ++ "\ESC[39m"
      where
        s' = replicate (n - length s) ' ' ++ s
    widths :: [Int]
    widths =
      foldl'
        ( \acc (RowGroup label rows) ->
            foldl'
              ( \acc2 -> \case
                  Row [] -> error "empty row"
                  Row (Cell _ s0 : cols) ->
                    zipWith max (max (length label) (length s0) : map (\(Cell _ s) -> length s) cols) acc2
                  Empty -> acc2
              )
              acc
              rows
        )
        (map (subtract 1 . length) labels)
        rowGroups
