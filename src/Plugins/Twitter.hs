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
  { pluginId = "tw/" `T.append` account
  , fetcher = fetcher
  , updater = updater
  }

  where
    fetcher :: BChan Card -> IO ()
    fetcher chan = do
      twInfo <- getTWInfo account
      manager <- newManager tlsManagerSettings
      runResourceT $ do
        src <- stream twInfo manager userstream
        src C.$$+- C.mapM_ (lift . fromStream chan)

      where
        fromStream :: BChan Card -> StreamingAPI -> IO ()
        fromStream chan = \case
          SStatus tw -> writeBChan chan $ renderStatus account tw
          _ -> return ()

        renderStatus :: T.Text -> Status -> Card
        renderStatus account tw
          = Card
            ("tw/" `T.append` account)
            ((tw ^. user ^. name) `T.append` " @" `T.append` (tw ^. user ^. screen_name))
            (tw ^. text)

    updater :: [T.Text] -> IO ()
    updater tw = do
      twInfo <- getTWInfo account
      manager <- newManager tlsManagerSettings
      call twInfo manager $ update $ T.unlines tw
      return ()

