module Plugins.Twitter where

import Brick.BChan
import Brick.Markup ((@?))
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans (lift)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import Data.Text.Markup
import Data.Monoid
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
        space_end = to $ \x -> x `T.append` " "

        fromStream :: BChan Card -> StreamingAPI -> IO ()
        fromStream chan = \case
          SStatus tw -> writeBChan chan $ renderStatus account tw
          SRetweetedStatus rtw -> writeBChan chan $ renderStatus account (rtw^.rsRetweetedStatus)
            & title %~ (((rtw^.rsUser^.screen_name^.at_) @? "screen-name" <> " retweeted ") <>)
          SEvent ev | ev ^. evEvent == "favorite" -> case (ev^.evSource, ev^.evTargetObject) of
            (ETUser u, Just (ETStatus s)) -> writeBChan chan $ renderStatus account s
              & title %~ (((u^.screen_name^.at_) @? "screen-name" <> " liked ") <>)
            _ -> return ()
          _ -> return ()

        renderStatus :: T.Text -> Status -> Card
        renderStatus account tw
          = Card
            twplugin
            (mconcat
             [ maysurround "(" (")" ^. space_end) aux @? "aux"
             , (tw ^. user ^. name ^. space_end) @? "user-name"
             , (tw ^. user ^. screen_name ^. at_ ^. space_end) @? "screen-name"
             ])
            (tw ^. text)
            (Just $ T.unlines $ filter (/= "") $
            [ tw ^. text
            , "------------"
            , (tw ^. statusCreatedAt ^. to show ^. to T.pack)
            , "★" `T.append` (tw ^. statusFavoriteCount ^. to show ^. to T.pack) `T.append` "  " `T.append` "🔃" `T.append` (tw ^. statusRetweetCount ^. to show ^. to T.pack)
            , maybe "" (\e -> e ^. enHashTags ^.. each . entityBody . hashTagText . to ("#" `T.append`) ^. to T.unwords) (tw ^. statusEntities)
            , maybe "" (\e -> e ^. enMedia ^.. each . entityBody . to (\m -> (m ^. meType) `T.append` ":" `T.append` (m ^. meMediaURL)) ^. to T.unlines) (tw ^. statusEntities)
            , maybe "" (\e -> e ^. enURLs ^.. each . entityBody . ueExpanded ^. to T.unlines) (tw ^. statusEntities)
            ]
            )

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

