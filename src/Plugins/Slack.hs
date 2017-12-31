{-# LANGUAGE QuasiQuotes #-}
module Plugins.Slack where

import Brick.BChan
import Brick.Markup ((@?))
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Web.Slack as Slack
import Types

slack :: T.Text -> BChan Item -> Plugin
slack account chan
  = Plugin
  { pluginId = pluginId
  , fetcher = fetcher
  , updater = \_ -> return ()
  , replyTo = \_ -> Nothing
  , keyRunner = M.fromList []
  , loadThread = \_ -> return ()
  }

  where
    pluginId = PluginId "slack" account

    fetcher :: IO ()
    fetcher = do
      val <- runAuth pluginId
      Slack.runBot (Slack.SlackConfig $ val ^?! key "access-token" . _String . to T.unpack) go ()

      where
        go :: Slack.Event -> Slack.Slack () ()
        go (Slack.Message chanid subm tx sts _ _) = do
          liftIO $ writeBChan chan $ ItemCard $ Card
            { _cardId = CardId pluginId (sts ^. to show . to T.pack)
            , _speaker = subm ^. to show . to T.pack
            , _title = (subm ^. to show . to T.pack) @? "slack-title"
            , _summary = tx
            , _content = Nothing
            , _labelCard = []
            , _inreplyto = Nothing
            }
        go _ = return ()

