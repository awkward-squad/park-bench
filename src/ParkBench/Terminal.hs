module ParkBench.Terminal
  ( withTerminal,
  )
where

import Control.Exception (bracket_)
import qualified Data.ByteString as ByteString
import ParkBench.Prelude
import System.IO (hSetEcho, stdin)

withTerminal :: IO a -> IO a
withTerminal =
  bracket_ hideCursor showCursor .
    bracket_ (hSetEcho stdin False) (hSetEcho stdin True)

hideCursor :: IO ()
hideCursor =
  ByteString.putStr "\ESC[?25l"

showCursor :: IO ()
showCursor =
  ByteString.putStr "\ESC[?25h"
