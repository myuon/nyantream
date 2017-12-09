module Main where

import Brick
import Brick.BChan
import qualified Brick.Widgets.List as W
import qualified Graphics.Vty as Vty
import Control.Lens
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Vector as V
import qualified Data.Text as T

import Types
import Plugins.Timer

app :: App Client Card String
app = App
  renderer
  (\_ _ -> Nothing)
  evhandler
  return
  (\_ -> attrMap (fg Vty.white) colorscheme)

  where
    renderer cli = return $ vBox
      [ W.renderList renderCard True (cli ^. timeline)
      , withAttr "inverted" $ padRight Max $ txt "--- *timeline* [sys/tw/tm]"
      , txt " "
      ]

    evhandler cli = \case
      VtyEvent (Vty.EvKey (Vty.KChar 'q') []) -> halt cli
      VtyEvent evkey -> handleEventLensed cli timeline W.handleListEvent evkey >>= continue
      AppEvent card -> do
        continue $ cli &~ do
          timeline %= W.listInsert (cli ^. timeline ^. W.listElementsL ^. to length - 1) card

          let Just (_,sel) = cli ^. timeline ^. to W.listSelectedElement
          when (sel ^. title == "sentinel") $ timeline %= W.listMoveDown
      _ -> continue cli

    colorscheme =
      [ ("inverted", Vty.black `on` Vty.white)
      ]

main :: IO ()
main = do
  size <- Vty.displayBounds =<< Vty.outputForConfig =<< Vty.standardIOConfig
  let cli = Client size (W.list "timeline" (V.fromList [Card "tw" "みょん @myuon_myon" "てすと", Card "sys" "sentinel" "--- fetching new cards ---"]) 2)
  chan <- newBChan 2

  forkIO $ fetch chan

  customMain
    (Vty.standardIOConfig >>= Vty.mkVty)
    (Just chan)
    app
    cli

  return ()

