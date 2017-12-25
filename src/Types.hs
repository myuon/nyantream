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

data PluginId = PluginId T.Text T.Text deriving (Eq, Ord)
data CardId = CardId PluginId T.Text deriving (Eq, Ord)

makePrisms ''PluginId
makePrisms ''CardId

data Card
  = Card
  { _cardId :: CardId
  , _speaker :: T.Text
  , _title :: Markup AttrName
  , _summary :: T.Text
  , _content :: Maybe T.Text
  , _label :: [T.Text]
  , _inreplyto :: Maybe CardId
  }

makeLenses ''Card

data Event
  = Event
  { _eventType :: T.Text
  , _ref :: CardId
  , _display :: Markup AttrName -> Markup AttrName
  }

makeLenses ''Event

data Item = ItemCard Card | ItemEvent Event

makePrisms ''Item

isCard :: Item -> Bool
isCard (ItemCard _) = True
isCard (ItemEvent _) = False

itemId :: Getter Item CardId
itemId = to $ \case
  (ItemCard card) -> card^.cardId
  (ItemEvent event) -> event^.ref

txtWrapper :: Int -> T.Text -> Widget n
txtWrapper w tx = vBox $ fmap vBox $ fmap (fmap txt . reverse . T.foldl go []) $ T.lines tx where
  go :: [T.Text] -> Char -> [T.Text]
  go [] ch = [T.singleton ch]
  go (x:xs) ch
    | textWidth (T.snoc x ch) >= w = T.singleton ch:x:xs
    | otherwise = T.snoc x ch:xs

textPluginId :: PluginId -> T.Text
textPluginId (PluginId x y)
  | y == "" = x
  | otherwise = x `T.append` "/" `T.append` y

renderCardWithIn :: Int -> Bool -> Card -> Widget n
renderCardWithIn w selected card =
  withAttr "plugin-id" (txt $ "[" `T.append` (card^.cardId^._CardId^._1^.to textPluginId) `T.append` "]") <+> txt " " <+> markup (card^.title)
  <=> withAttr ((if selected then ("inverted" <>) else id) "card-content") (padRight Max $ txtWrapper w (card ^. summary))

renderDetailCardWithIn :: Int -> Bool -> Card -> Widget n
renderDetailCardWithIn w selected card =
  withAttr "plugin-id" (txt $ "[" `T.append` (card^.cardId^._CardId^._1^.to textPluginId) `T.append` "]") <+> txt " " <+> markup (card^.title)
  <=> withAttr ((if selected then ("inverted" <>) else id) "card-content") (padRight Max $ txtWrapper w (maybe (card ^. summary) id (card ^. content)))

data Plugin
  = Plugin
  { pluginId :: PluginId
  , fetcher :: IO ()
  , updater :: [T.Text] -> IO ()
  , replyTo :: Card -> Maybe ReplyInfo
  , keyRunner :: M.Map Char (Item -> IO ())
  , loadThread :: Card -> IO ()
  }

data ReplyInfo
  = ReplyInfo
  { placeholder :: T.Text
  , description :: T.Text
  , replyUpdater :: [T.Text] -> IO ()
  }

runAuth :: PluginId -> IO (Maybe Value)
runAuth pluginId = do
  let path = "token/" ++ T.unpack (pluginId^.to textPluginId)
--  createDirectoryIfMissing True path
  decodeStrict <$> S8.readFile path

