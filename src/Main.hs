module Main where

import Brick
import Brick.BChan
import qualified Brick.Widgets.List as W
import qualified Graphics.Vty as Vty
import Control.Lens
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Time

data Card
  = Card
  { _plugin :: T.Text
  , _title :: T.Text
  , _content :: T.Text
  }

makeLenses ''Card

renderCard :: Bool -> Card -> Widget n
renderCard _ card =
  txt "[" <+> txt (card ^. plugin) <+> txt "] " <+> txt (card ^. title)
  <=> txt (card ^. content)

data Client
  = Client
  { _timeline :: W.List String Card
  }

makeLenses ''Client

app :: App Client Card String
app = App
  renderer
  (\_ _ -> Nothing)
  evhandler
  return
  (\_ -> attrMap (fg Vty.white) [])

  where
    renderer cli =
      [ W.renderList renderCard True (cli ^. timeline)
      ]

    evhandler cli = \case
      VtyEvent (Vty.EvKey (Vty.KChar 'q') []) -> halt cli
      AppEvent card -> continue $ cli & timeline %~ W.listInsert (cli ^. timeline ^. W.listElementsL ^. to length) card
      _ -> continue cli

fetch :: BChan Card -> IO ()
fetch chan = do
  threadDelay 1000000
  time <- getCurrentTime
  writeBChan chan $ Card "tw" "timer" (T.pack $ show time)
  fetch chan

main :: IO ()
main = do
  let cli = Client (W.list "timeline" (V.fromList [Card "tw" "みょん @myuon_myon" "てすと"]) 2)
  chan <- newBChan 2

  forkIO $ fetch chan

  customMain
    (Vty.standardIOConfig >>= Vty.mkVty)
    (Just chan)
    app
    cli

  return ()

