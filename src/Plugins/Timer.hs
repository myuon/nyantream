module Plugins.Timer where

import Brick.BChan
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import Data.Time
import Data.Time.Lens
import Types

-- hourly
hscheduler :: Plugin
hscheduler
  = Plugin
  { pluginId = "hscheduler"
  , fetcher = fetcher
  , updater = \_ -> return ()
  }

  where
    fetcher chan = forever $ do
      cur <- getZonedTime
      let next = cur & flexDT . hours +~ 1 & flexDT . minutes .~ 0 & flexDT . seconds .~ 0
      writeBChan chan $ Card "tm" "timer" (T.pack $ "current time: " ++ show cur ++ "\nnext: " ++ show next)
      threadDelay $ fromInteger $ (`div` (1000 * 1000)) $ diffTimeToPicoseconds $ (next ^. timeAsDiff - cur ^. timeAsDiff)
      go

      where
        go = do
          cur <- getZonedTime
          writeBChan chan $ Card "tm" "timer" (T.pack $ "current time: " ++ show cur)
          threadDelay $ 1000 * 1000 * 60 * 60


