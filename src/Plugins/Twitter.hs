module Plugins.Twitter where

import Brick.BChan
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans (lift)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import Web.Authenticate.OAuth as OA
import Web.Twitter.Conduit hiding (lookup,url)
import Web.Twitter.Types.Lens
import Types

getTWInfo :: T.Text -> IO TWInfo
getTWInfo account = do
  xs <- S8.lines <$> S8.readFile ("token/" ++ T.unpack account)

  return $ setCredential
    (twitterOAuth
     { oauthConsumerKey = xs!!0
     , oauthConsumerSecret = xs!!1 })
    (Credential
     [ ("oauth_token", xs!!2)
     , ("oauth_token_secret", xs!!3) ])
    def

twitter :: T.Text -> Plugin
twitter account
  = Plugin
  { pluginId = twplugin
  , fetcher = fetcher
  , updater = updater
  }

  where
    twplugin = "tw/" `T.append` account

    fetcher :: BChan Card -> IO ()
    fetcher chan = do
      twInfo <- getTWInfo account
      manager <- newManager tlsManagerSettings
      runResourceT $ do
        src <- stream twInfo manager userstream
        src C.$$+- C.mapM_ (lift . fromStream chan)

      where
        at_ = to $ \x -> "@" `T.append` x

        fromStream :: BChan Card -> StreamingAPI -> IO ()
        fromStream chan = \case
          SStatus tw -> writeBChan chan $ renderStatus account tw
          SRetweetedStatus rtw -> writeBChan chan $ renderStatus account (rtw^.rsRetweetedStatus) & title %~ T.append ((rtw^.rsUser^.screen_name^.at_) `T.append` " retweeted ")
          SEvent ev | ev ^. evEvent == "favorite" -> case (ev^.evSource, ev^.evTargetObject) of
            (ETUser u, Just (ETStatus s)) -> writeBChan chan $ renderStatus account s & title %~ T.append ((u ^. screen_name) `T.append` " liked ")
            _ -> return ()
          _ -> return ()

        renderStatus :: T.Text -> Status -> Card
        renderStatus account tw
          = Card
            twplugin
            (T.strip $ T.unwords
             [ maysurround "(" ")" aux
             , tw ^. user ^. name
             , tw ^. user ^. screen_name ^. at_
             ])
            (tw ^. text)

          where
            maysurround st ed xs = if T.null xs then "" else st `T.append` xs `T.append` ed

            aux = T.concat
              [ tw ^. statusFavorited ^. _Just . to (\b -> if b then "★" else "")
              , tw ^. statusRetweeted ^. _Just . to (\b -> if b then "🔃" else "")
              , tw ^. statusInReplyToStatusId ^. _Just . to (const "▷")
              ]

    updater :: [T.Text] -> IO ()
    updater tw = do
      twInfo <- getTWInfo account
      manager <- newManager tlsManagerSettings
      call twInfo manager $ update $ T.unlines tw
      return ()

