module Plugins.Timer where

import Brick.BChan
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import qualified Data.Map as M
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
  , keyRunner = M.empty
  }

  where
    fetcher chan = forever $ do
      cur <- getZonedTime
      let next = cur & flexDT . hours +~ 1 & flexDT . minutes .~ 0 & flexDT . seconds .~ 0
      writeBChan chan $ Card "tm" "" "timer" (T.pack $ "current time: " ++ show cur ++ "\nnext: " ++ show next) Nothing
      threadDelay $ fromInteger $ (`div` (1000 * 1000)) $ diffTimeToPicoseconds $ (next ^. timeAsDiff - cur ^. timeAsDiff)
      go

      where
        go = do
          cur <- getZonedTime
          writeBChan chan $ Card "tm" "" "timer" (T.pack $ "current time: " ++ show cur) Nothing
          threadDelay $ 1000 * 1000 * 60 * 60


