module Client where

import Brick
import Brick.BChan
import qualified Brick.Widgets.List as W
import qualified Brick.Widgets.Edit as W
import qualified Graphics.Vty as Vty
import Control.Lens
import Control.Monad
import Control.Monad.Coroutine
import Control.Concurrent
import qualified Data.Vector as V
import qualified Data.Text as T

import Types
import Plugins.Timer
import Plugins.Twitter

data FocusOn = Timeline | Minibuffer
  deriving (Eq, Show)

data Client
  = Client
  { _size :: (Int,Int)
  , _focusing :: FocusOn
  , _timeline :: W.List String Card
  , _minibuffer :: W.Editor T.Text String
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
    renderer cli = return $ vBox
      [ W.renderList renderCard (cli^.focusing == Timeline) (cli ^. timeline)
      , withAttr "inverted" $ padRight Max $ txt $ "--- *" `T.append` T.pack (show $ cli ^. focusing) `T.append` "* [sys/tw/tm]"
      , W.renderEditor (cli^.focusing == Minibuffer) (cli ^. minibuffer)
      ]

    evhandler cli = \case
      VtyEvent (Vty.EvKey (Vty.KChar 'q') []) -> halt cli
      VtyEvent (Vty.EvKey (Vty.KChar 'x') [Vty.MMeta]) -> continue $ cli & focusing .~ Minibuffer
      VtyEvent evkey -> case cli^.focusing of
        Timeline -> handleEventLensed cli timeline W.handleListEvent evkey >>= continue
        Minibuffer -> handleEventLensed cli minibuffer W.handleEditorEvent evkey >>= continue
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
    (W.editorText "minibuffer" (vBox . fmap txt) (Just 1) "")

type Plugin = BChan Card -> IO ()

runClient :: [Plugin] -> IO ()
runClient plugins = do
  size <- Vty.displayBounds =<< Vty.outputForConfig =<< Vty.standardIOConfig
  chan <- newBChan 2
  mapM_ (\p -> forkIO $ p chan) plugins

  customMain
    (Vty.standardIOConfig >>= Vty.mkVty)
    (Just chan)
    app
    (defClient size)

  return ()

