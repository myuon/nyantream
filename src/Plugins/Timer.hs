module Plugins.Timer where

import Brick.BChan
import qualified Data.Text as T
import Data.Time
import Control.Concurrent (threadDelay)
import Types

fetch :: BChan Card -> IO ()
fetch chan = do
  threadDelay 1000000
  time <- getCurrentTime
  writeBChan chan $ Card "tm" "timer" (T.pack $ show time)
  fetch chan


