{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
module Plugins.Gmail where

import Brick.BChan
import Brick.Markup ((@?))
import Codec.MIME.Base64 (decodeToString)
import Codec.Binary.UTF8.String (decodeString)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import Network.OAuth.OAuth2 hiding (error)
import Network.HTTP.Conduit
import Network.Google.Gmail.Types
import URI.ByteString
import URI.ByteString.QQ
import Unsafe.Coerce (unsafeCoerce)
import GHC.Word (Word64)
import Types

decodeMessage :: MessagePart -> String
decodeMessage mp
  | isJust (mp ^. mpBody) = mp ^. mpBody ^?! _Just ^. mpbData ^?! _Just ^. to S8.unpack ^. to (replace '-' '+') ^. to (replace '_' '/') ^. to decodeToString ^. to decodeString ^.. folded . filtered (/= '\r')
  | otherwise = unlines $ fmap decodeMessage (mp ^. mpParts)
  where
    replace c c' xs = xs >>= \x -> if x == c then [c'] else [x]

gmail :: T.Text -> Plugin
gmail account
  = Plugin
  { pluginId = pluginId
  , fetcher = fetcher
  , updater = \_ -> error "not implemented"
  , keyRunner = M.empty
  }
  where
  pluginId = "gm/" `T.append` account

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

    let getter :: forall a. FromJSON a => URI -> IO (OAuth2Result T.Text a)
        getter = authGetJSON @T.Text mgr (accessToken token)

    Right lmr <- getter @ListMessagesResponse [uri|https://www.googleapis.com/gmail/v1/users/me/messages?maxResults=1|]
    Right m <- getter @Message ([uri|https://www.googleapis.com/gmail/v1/users/me/messages/|] & pathL <>~ (lmr ^. lmrMessages ^. to (!! 0) ^. mId ^?! _Just ^. to T.unpack ^. to S8.pack))

    go getter (m ^. mHistoryId ^?! _Just)

    where
      renderMessage :: Message -> Card
      renderMessage msg = Card
        pluginId
        (msg ^. mId ^?! _Just)
        ((msg ^. mPayload ^?! _Just ^. mpHeaders ^. to (fmap (\t -> (t ^. mphName ^?! _Just, t ^. mphValue ^?! _Just))) ^. to (lookup "Subject") ^?! _Just) @? "mail-subject")
        (msg ^. mSnippet ^?! _Just)
        (Just $ msg ^. mPayload ^?! _Just ^. to decodeMessage ^. to T.pack)
        (if "IMPORTANT" `elem` msg ^. mLabelIds then ["notify"] else [])

      go :: (forall a. FromJSON a => URI -> IO (OAuth2Result T.Text a)) -> Word64 -> IO ()
      go getter hid = do
        Right lhr <- getter @ListHistoryResponse ([uri|https://www.googleapis.com/gmail/v1/users/me/history?historyTypes=messageAdded|] & queryL . queryPairsL <>~ [("startHistoryId", S8.pack $ show hid)])
        forM_ (lhr ^. lhrHistory ^.. each . hMessagesAdded . each . hmaMessage ^. to catMaybes ^.. each . mId ^. to catMaybes) $ \mid -> do
          Right msg <- getter @Message ([uri|https://www.googleapis.com/gmail/v1/users/me/messages/|] & pathL <>~ (S8.pack $ T.unpack mid))
          writeBChan chan $ renderMessage msg
        threadDelay $ 1000 * 1000 * 60
        go getter $ lhr ^. lhrHistoryId ^?! _Just

