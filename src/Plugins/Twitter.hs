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
import qualified Data.Map as M
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
  , replyTo = replyTo
  , keyRunner = M.fromList [('f', favo)]
  }

  where
    at_ = to $ \x -> "@" `T.append` x
    space_end = to $ \x -> x `T.append` " "

    replyTo :: Card -> Maybe ReplyInfo
    replyTo card = Just
      (ReplyInfo
        { placeholder = card^.speaker^.at_^.space_end
        , description = rtext
        , replyUpdater = replyUpdater
        })

      where
        rtext = mconcat
          [ "reply to: "
          , card^.speaker
          , " {"
          , card^.cardId^._CardId^._2
          , "}"
          ]
        replyUpdater msg = do
          twInfo <- getTWInfo account
          manager <- newManager tlsManagerSettings
          call twInfo manager $ update (T.unlines msg) & inReplyToStatusId ?~ card^.cardId^._CardId^._2^.to T.unpack^.to read
          return ()

    favo :: Card -> IO ()
    favo c = do
      twInfo <- getTWInfo account
      manager <- newManager tlsManagerSettings
      call twInfo manager $ favoritesCreate (c^.cardId^._CardId^._2^.to T.unpack^.to read)
      return ()

    twplugin = PluginId "tw" account

    fetcher :: BChan Card -> IO ()
    fetcher chan = do
      twInfo <- getTWInfo account
      manager <- newManager tlsManagerSettings
      runResourceT $ do
        src <- stream twInfo manager userstream
        src C.$$+- C.mapM_ (lift . fromStream chan)

      where
        fromStream :: BChan Card -> StreamingAPI -> IO ()
        fromStream chan api = case api of
          SStatus tw -> writeBChan chan $ renderStatus account tw (shouldNotify api)
          SRetweetedStatus rtw -> writeBChan chan $ renderStatus account (rtw^.rsRetweetedStatus) (shouldNotify api)
            & title %~ (((rtw^.rsUser^.screen_name^.at_) @? "screen-name" <> " retweeted ") <>)
          SEvent ev | ev ^. evEvent == "favorite" -> case (ev^.evSource, ev^.evTargetObject) of
            (ETUser u, Just (ETStatus s)) -> writeBChan chan $ renderStatus account s (shouldNotify api)
              & title %~ (((u^.screen_name^.at_) @? "screen-name" <> " liked ") <>)
            _ -> return ()
          _ -> return ()

        shouldNotify :: StreamingAPI -> Bool
        shouldNotify = \case
          SStatus tw -> tw ^. statusInReplyToScreenName == Just account
          SRetweetedStatus rtw -> rtw ^. rsRetweetedStatus ^. user ^. screen_name == account
          SEvent ev | ev ^. evEvent == "favorite" -> case (ev^.evSource, ev^.evTargetObject) of
            (ETUser u, Just (ETStatus s)) -> s ^. user ^. screen_name == account
          _ -> False

        renderStatus :: T.Text -> Status -> Bool -> Card
        renderStatus account tw notify
          = Card
          { _cardId = CardId twplugin $ tw ^. statusId ^. to show ^. to T.pack
          , _speaker = tw ^. user ^. screen_name
          , _title = mconcat
            [ aux @? "aux"
            , (tw ^. user ^. name ^. space_end) @? "user-name"
            , (tw ^. user ^. screen_name ^. at_ ^. space_end) @? "screen-name"
            ]
          , _summary = tw ^. text
          , _content = Just $ T.unlines $ filter (/= "") $
            [ tw ^. text
            , "------------"
            , (tw ^. statusCreatedAt ^. to show ^. to T.pack)
            , "★" `T.append` (tw ^. statusFavoriteCount ^. to show ^. to T.pack) `T.append` "  " `T.append` "🔃" `T.append` (tw ^. statusRetweetCount ^. to show ^. to T.pack)
            , maybe "" (\e -> e ^. enHashTags ^.. each . entityBody . hashTagText . to ("#" `T.append`) ^. to T.unwords) (tw ^. statusEntities)
            , maybe "" (\e -> e ^. enMedia ^.. each . entityBody . to (\m -> (m ^. meType) `T.append` ":" `T.append` (m ^. meMediaURL)) ^. to T.unlines) (tw ^. statusEntities)
            , maybe "" (\e -> e ^. enURLs ^.. each . entityBody . ueExpanded ^. to T.unlines) (tw ^. statusEntities)
            ]
          , _label = if notify then ["notify"] else []
          }

          where
            maysurround st ed xs = if T.null xs then "" else st `T.append` xs `T.append` ed

            aux = T.concat
              [ tw ^. statusFavorited ^. _Just . to (\b -> if b then "★" else "")
              , tw ^. statusRetweeted ^. _Just . to (\b -> if b then "🔃" else "")
              , tw ^. statusInReplyToStatusId ^. _Just . to (const "▷ ")
              ]

    updater :: [T.Text] -> IO ()
    updater tw = do
      twInfo <- getTWInfo account
      manager <- newManager tlsManagerSettings
      call twInfo manager $ update $ T.unlines tw
      return ()

