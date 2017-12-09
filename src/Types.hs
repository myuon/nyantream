module Types where

import Brick
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

