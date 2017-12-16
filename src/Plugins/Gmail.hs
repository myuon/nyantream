{-# LANGUAGE QuasiQuotes #-}
module Plugins.Gmail where

import Brick.BChan
import Brick.Markup ((@?))
import Control.Lens
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.Aeson (Value)
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import Data.Maybe (catMaybes)
import Network.OAuth.OAuth2
import Network.HTTP.Conduit
import URI.ByteString
import URI.ByteString.QQ
import Unsafe.Coerce
import Types

mailGoogleScope = "https://mail.google.com/"

gmail :: T.Text -> Plugin
gmail account
  = Plugin
  { pluginId = pluginId
  , fetcher = fetcher
  , updater = undefined
  }
  where
  pluginId = "gmail/" `T.append` account

  buildOAuth :: IO OAuth2
  buildOAuth = do
    value <- runAuth pluginId
    return $ OAuth2
      { oauthClientId = value ^. key "client_id" ^?! _Just
      , oauthClientSecret = value ^. key "client_secret" ^?! _Just
      , oauthOAuthorizeEndpoint = [uri|https://accounts.google.com/o/oauth2/v2/auth|]
      , oauthAccessTokenEndpoint = [uri|https://www.googleapis.com/oauth2/v4/token|]
      , oauthCallback = Just (unsafeCoerce "urn:ietf:wg:oauth:2.0:oob")
      }

  fetcher :: BChan Card -> IO ()
  fetcher chan = do
    mgr <- newManager tlsManagerSettings
    value <- runAuth pluginId
    googleOAuth <- buildOAuth
    Right token <- fetchRefreshToken mgr googleOAuth (value ^. key "refresh_token" ^?! _Just)
    let getter = authGetJSON @Value @Value mgr (accessToken token)
    Right v <- getter [uri|https://www.googleapis.com/gmail/v1/users/me/messages?maxResults=1|]
    Right v <- getter ([uri|https://www.googleapis.com/gmail/v1/users/me/messages/|] & pathL <>~ (Just v ^. key "messages" ^. nth 0 ^. key "id" ^?! _Just ^. to S8.pack))

    go getter (Just v ^. key "historyId" ^?! _Just)

    where
      renderMessage :: Value -> Card
      renderMessage v = Card
        pluginId
        ((Just v ^. key "payload" ^. key "headers" ^.. traverseArray . each . to (\v -> (v ^. key "name" ^?! _Just, v ^. key "value" ^?! _Just)) ^. to (lookup "Subject" :: [(T.Text,T.Text)] -> Maybe T.Text) ^?! _Just) @? "mail-subject")
        (Just v ^. key "payload" ^. key "parts" ^.. traverseArray . asText ^. to catMaybes ^. to T.unlines)

      go getter hid = do
        Right v <- getter ([uri|https://www.googleapis.com/gmail/v1/users/me/history?historyTypes=messageAdded|] & queryL . queryPairsL <>~ [("startHistoryId", S8.pack hid)])
        let ids = Just v ^. key "history" ^.. traverseArray . key "messagesAdded" ^.. each . traverseArray . key "id" . asText ^. to catMaybes :: [T.Text]
        forM_ ids $ \mid -> do
          Right v <- getter ([uri|https://www.googleapis.com/gmail/v1/users/me/messages/|] & pathL <>~ (S8.pack $ T.unpack mid))
          writeBChan chan $ renderMessage v
        threadDelay $ 1000 * 1000 * 60
        go getter $ Just v ^. key "historyId" ^?! _Just

