{-# LANGUAGE DeriveFunctor #-}
module Types where

import Brick
import Brick.BChan
import Brick.Markup
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid
import Data.Text.Markup
import System.Directory (createDirectoryIfMissing)

data Card
  = Card
  { _pluginOf :: T.Text
  , _title :: Markup AttrName
  , _content :: T.Text
  }

makeLenses ''Card

txtWrapper :: Int -> T.Text -> Widget n
txtWrapper w tx = vBox $ fmap txt $ reverse $ T.foldl go [] tx where
  go :: [T.Text] -> Char -> [T.Text]
  go [] ch = [T.singleton ch]
  go (x:xs) ch
    | textWidth (T.snoc x ch) >= w = T.singleton ch:x:xs
    | otherwise = T.snoc x ch:xs

renderCardWithIn :: Int -> Bool -> Card -> Widget n
renderCardWithIn w selected card =
  withAttr "plugin-id" (txt $ "[" `T.append` (card^.pluginOf) `T.append` "]") <+> txt " " <+> markup (card^.title)
  <=> withAttr ((if selected then ("inverted" <>) else id) "card-content") (padRight Max $ txtWrapper w (card ^. content))

data Plugin
  = Plugin
  { pluginId :: T.Text
  , fetcher :: BChan Card -> IO ()
  , updater :: [T.Text] -> IO ()
  }

runAuth :: T.Text -> IO (Maybe Value)
runAuth pluginId = do
  let path = "token/" ++ T.unpack pluginId
--  createDirectoryIfMissing True path
  decodeStrict <$> S8.readFile path

