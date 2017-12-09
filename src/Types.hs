module Types where

import Brick
import qualified Brick.Widgets.List as W
import Control.Lens
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

data Client
  = Client
  { _size :: (Int,Int)
  , _timeline :: W.List String Card
  }

makeLenses ''Client


