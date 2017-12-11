module Client where

import Brick
import Brick.BChan
import qualified Brick.Widgets.Border as W
import qualified Brick.Widgets.Center as W
import qualified Brick.Widgets.List as W
import qualified Brick.Widgets.Edit as W
import qualified Graphics.Vty as Vty
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Text.Zipper (clearZipper)

import Types

data FocusOn = Timeline | Minibuffer | Textarea | Item
  deriving (Eq, Show)

data Client
  = Client
  { _size :: (Int,Int)
  , _focusing :: FocusOn
  , _timeline :: W.List String Card
  , _minibuffer :: W.Editor T.Text String
  , _textarea :: W.Editor T.Text String
  , _plugins :: (Int, [Plugin])
  }

makeLenses ''Client

app :: App Client Card String
app = App
  renderer
  showFirstCursor
  evhandler
  return
  (\_ -> attrMap (fg Vty.white) colorscheme)

  where
    renderer cli | cli^.focusing `elem` [Timeline, Minibuffer] = return $ vBox
      [ W.renderList (renderCardWithIn (cli ^. size ^. _1)) (cli^.focusing == Timeline) (cli ^. timeline)
      , withAttr "inverted" $ padRight Max $ txt $ "--- *" `T.append` T.pack (show $ cli ^. focusing) `T.append` "* [sys/tw/tm]"
      , W.renderEditor (cli^.focusing == Minibuffer) (cli ^. minibuffer)
      ]
    renderer cli | cli^.focusing == Item = return $ vBox
      [ translateBy (Location (0,4)) $ W.hCenterLayer $ W.border $ padLeftRight 1 $ renderCardWithIn (cli^.size^._1^.to fromIntegral^.to (* 0.7)^.to floor) (cli^.focusing == Timeline) (cli^.timeline^.to W.listSelectedElement^?!_Just^._2)
      ]
    renderer cli | cli^.focusing == Textarea = return $ vBox
      [ vLimit (cli ^. size ^. _2 - 6) $ W.renderList (renderCardWithIn (cli ^. size ^. _1)) (cli^.focusing == Timeline) (cli ^. timeline)
      , withAttr "inverted" $ padRight Max $ txt $ "--- [" `T.append` (cli ^. plugins ^. _2 ^? ix (cli ^. plugins ^. _1) ^. _Just . to pluginId) `T.append` "] *textarea* (C-c)send (C-q)quit (C-n)next plugin"
      , vLimit 5 $ W.renderEditor (cli^.focusing == Textarea) (cli ^. textarea)
      ]

    evhandler cli = \case
      VtyEvent evkey -> case cli^.focusing of
        Timeline -> case evkey of
          Vty.EvKey (Vty.KChar 'q') [] -> halt cli
          Vty.EvKey (Vty.KChar 'x') [Vty.MMeta] -> continue $ cli & focusing .~ Minibuffer
          Vty.EvKey (Vty.KChar 'a') [Vty.MCtrl] -> continue $ cli & focusing .~ Textarea
          Vty.EvKey (Vty.KChar 'i') [] -> continue $ cli & focusing .~ Item
          _ -> handleEventLensed cli timeline W.handleListEvent evkey >>= continue
        Minibuffer -> case evkey of
          Vty.EvKey (Vty.KChar 'g') [Vty.MCtrl] -> continue $ cli & focusing .~ Timeline
          _ -> handleEventLensed cli minibuffer W.handleEditorEvent evkey >>= continue
        Item -> case evkey of
          Vty.EvKey _ _ -> continue $ cli & focusing .~ Timeline
          _ -> continue cli
        Textarea -> case evkey of
          Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl] -> continue $ cli & focusing .~ Timeline
          Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> do
            liftIO (cli^.plugins^._2^?ix (cli^.plugins^._1)^._Just.to (flip updater (cli^.textarea^.to W.getEditContents)))
            continue $ cli & focusing .~ Timeline & textarea . W.editContentsL %~ clearZipper
          Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl] | cli^.plugins^._2^.to length /= 0 -> continue $ cli & plugins . _1 %~ (\x -> (x+1) `mod` (length $ cli^.plugins^._2))
          _ -> handleEventLensed cli textarea W.handleEditorEvent evkey >>= continue
      AppEvent card -> do
        continue $ cli &~ do
          timeline %= W.listInsert (cli ^. timeline ^. W.listElementsL ^. to length - 1) card

          let Just (_,sel) = cli ^. timeline ^. to W.listSelectedElement
          when (sel ^. title == sentinel ^. title) $ timeline %= W.listMoveDown
      _ -> continue cli

    colorscheme =
      [ ("inverted", Vty.black `on` Vty.white)
      ]

sentinel :: Card
sentinel = Card "sys" "sentinel" "--- fetching new cards ---"

defClient :: (Int,Int) -> Client
defClient s =
  Client
    s
    Timeline
    (W.list "timeline" (V.singleton sentinel) 2)
    (W.editorText "minibuffer" (txt . T.unlines) (Just 1) "")
    (W.editorText "textarea" (txt . T.unlines) (Just 5) "")
    (0,[])

runClient :: [Plugin] -> IO ()
runClient pls = do
  size <- Vty.displayBounds =<< Vty.outputForConfig =<< Vty.standardIOConfig
  chan <- newBChan 2
  mapM_ (\p -> forkIO $ fetcher p chan) pls

  customMain
    (Vty.standardIOConfig >>= Vty.mkVty)
    (Just chan)
    app
    (defClient size & plugins .~ (0,pls))

  return ()

