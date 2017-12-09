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

renderCard :: Bool -> Card -> Widget n
renderCard selected card =
  ((if selected then withAttr "inverted" else id) $ txt "[" <+> txt (card ^. plugin) <+> txt "] " <+> txt (card ^. title))
  <=> txt (card ^. content)

data Plugin r
  = AwaitLine (String -> r)
  | Async (BChan Card -> IO ()) r
  deriving Functor

awaitLine :: Monad m => Coroutine Plugin m String
awaitLine = suspend $ AwaitLine return

async :: Monad m => (BChan Card -> IO ()) -> Coroutine Plugin m ()
async m = suspend $ Async m (return ())

