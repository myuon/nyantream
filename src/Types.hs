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
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text.Markup
import System.Directory (createDirectoryIfMissing)

data Card
  = Card
  { _pluginOf :: T.Text
  , _cardId :: T.Text
  , _speaker :: T.Text
  , _title :: Markup AttrName
  , _summary :: T.Text
  , _content :: Maybe T.Text
  , _label :: [T.Text]
  }

makeLenses ''Card

txtWrapper :: Int -> T.Text -> Widget n
txtWrapper w tx = vBox $ fmap vBox $ fmap (fmap txt . reverse . T.foldl go []) $ T.lines tx where
  go :: [T.Text] -> Char -> [T.Text]
  go [] ch = [T.singleton ch]
  go (x:xs) ch
    | textWidth (T.snoc x ch) >= w = T.singleton ch:x:xs
    | otherwise = T.snoc x ch:xs

renderCardWithIn :: Int -> Bool -> Card -> Widget n
renderCardWithIn w selected card =
  withAttr "plugin-id" (txt $ "[" `T.append` (card^.pluginOf) `T.append` "]") <+> txt " " <+> markup (card^.title)
  <=> withAttr ((if selected then ("inverted" <>) else id) "card-content") (padRight Max $ txtWrapper w (card ^. summary))

renderDetailCardWithIn :: Int -> Bool -> Card -> Widget n
renderDetailCardWithIn w selected card =
  withAttr "plugin-id" (txt $ "[" `T.append` (card^.pluginOf) `T.append` "]") <+> txt " " <+> markup (card^.title)
  <=> withAttr ((if selected then ("inverted" <>) else id) "card-content") (padRight Max $ txtWrapper w (maybe (card ^. summary) id (card ^. content)))

data Plugin
  = Plugin
  { pluginId :: T.Text
  , fetcher :: BChan Card -> IO ()
  , updater :: [T.Text] -> IO ()
  , replyTo :: Card -> Maybe ReplyInfo
  , keyRunner :: M.Map Char (Card -> IO ())
  }

data ReplyInfo
  = ReplyInfo
  { placeholder :: T.Text
  , description :: T.Text
  , replyUpdater :: [T.Text] -> IO ()
  }

runAuth :: T.Text -> IO (Maybe Value)
runAuth pluginId = do
  let path = "token/" ++ T.unpack pluginId
--  createDirectoryIfMissing True path
  decodeStrict <$> S8.readFile path

