module ParkBench.Terminal
  ( withTerminal,
    clearFromCursor,
    cursorUp,
    blue,
    green,
    red,
  )
where

import Control.Exception (bracket_)
import qualified Data.ByteString as ByteString
import ParkBench.Builder (Builder)
import qualified ParkBench.Builder as Builder
import ParkBench.Prelude
import System.IO (hSetEcho, stdin)

withTerminal :: IO a -> IO a
withTerminal =
  bracket_ hideCursor showCursor . bracket_ (hSetEcho stdin False) (hSetEcho stdin True)

hideCursor :: IO ()
hideCursor =
  ByteString.putStr "\ESC[?25l"

showCursor :: IO ()
showCursor =
  ByteString.putStr "\ESC[?25h"

clearFromCursor :: Builder
clearFromCursor =
  "\ESC[0J"

cursorUp :: Int -> Builder
cursorUp n =
  "\ESC[" <> Builder.decimal n <> "F"

blue :: Builder -> Builder
blue s =
  "\ESC[34m" <> s <> "\ESC[39m"

green :: Builder -> Builder
green s =
  "\ESC[32m" <> s <> "\ESC[39m"

red :: Builder -> Builder
red s =
  "\ESC[31m" <> s <> "\ESC[39m"
