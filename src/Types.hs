{-# LANGUAGE DeriveFunctor #-}
module Types where

import Brick
import Brick.BChan
import Control.Lens
import Control.Monad.Coroutine
import qualified Data.Text as T

data Card
  = Card
  { _plugin :: T.Text
  , _title :: T.Text
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
  ((if selected then withAttr "inverted" else id) $ txt "[" <+> txt (card ^. plugin) <+> txt "] " <+> txt (card ^. title))
  <=> txtWrapper w (card ^. content)
