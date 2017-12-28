module Nyantream where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BS

import Plugins.Twitter
import Plugins.Timer
import Plugins.Gmail
import Client

main = do
  json <- BS.readFile "config.json"
  installedPlugins <- forM (json ^?! _JSON :: [Value]) $ \p -> do
    case p ^?! key "plugin" of
      "twitter" -> return $ twitter (p ^?! key "account" . _String)
      "gmail" -> return $ gmail (p ^?! key "account" . _String)
      "timer" -> return $ hscheduler

  runClient installedPlugins

