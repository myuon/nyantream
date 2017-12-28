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
hscheduler :: BChan Item -> Plugin
hscheduler chan
  = Plugin
  { pluginId = pluginId
  , fetcher = fetcher
  , updater = \_ -> return ()
  , replyTo = \_ -> Nothing
  , keyRunner = M.empty
  , loadThread = \_ -> return ()
  }

  where
    pluginId = PluginId "hschedular" ""

    renderTimer :: T.Text -> Card
    renderTimer content
      = Card
      { _cardId = CardId pluginId ""
      , _speaker = ""
      , _title = "timer"
      , _summary = content
      , _content = Nothing
      , _labelCard = []
      , _inreplyto = Nothing
      }

    fetcher = forever $ do
      cur <- getZonedTime
      let next = cur & flexDT . hours +~ 1 & flexDT . minutes .~ 0 & flexDT . seconds .~ 0
      writeBChan chan $ ItemCard $ renderTimer (T.pack $ "current time: " ++ show cur ++ "\nnext: " ++ show next)
      let n = fromInteger $ (`div` (1000 * 1000)) $ diffTimeToPicoseconds $ ((if (next ^. timeAsDiff == 0) then 60 * 60 * 24 else next ^. timeAsDiff) - cur ^. timeAsDiff)
      threadDelay n

