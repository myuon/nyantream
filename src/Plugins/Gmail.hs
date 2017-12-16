module Plugins.Gmail where

import Brick.BChan
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson.Lens
import qualified Data.Text as T
import Network.Google
import Network.Google.Auth
import Network.Google.Gmail
import System.IO (stdout)
import Types

gmail :: T.Text -> Plugin
gmail account
  = Plugin
  { pluginId = pluginId
  , fetcher = fetcher
  , updater = undefined
  }
  where
  pluginId = "gmail/" `T.append` account

  buildCred kv =
    installedApplication
    (OAuthClient (kv ^. key "client_id" ^?! _Just) (kv ^. key "client_secret" ^?! _Just))
    (OAuthCode (kv ^. key "oauth_code" ^?! _Just))

  fetcher :: BChan Card -> IO ()
  fetcher chan = do
    lgr <- newLogger Debug stdout
    mgr <- newManager tlsManagerSettings
    kv <- runAuth pluginId
    let cred@(FromClient z w) = buildCred kv
    Auth (FromClient x y) t <- refresh cred lgr mgr
    env <- newEnvWith (FromClient x y) lgr mgr <&> (envScopes .~ mailGoogleComScope)
    runResourceT . runGoogle env $ do
      ms <- send $ usersMessagesGet "ioijoikoiloi@gmail.com"
      liftIO $ print ms

