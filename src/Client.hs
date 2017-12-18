{-# LANGUAGE Rank2Types #-}
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
import qualified Data.Map as M
import Data.Monoid
import Data.Text.Zipper (clearZipper, textZipper)
import Data.Unique

import Types

data FocusOn
  = Timeline | Notification
  | Minibuffer
  | Compose | ReplyTo T.Text
  | Item
  deriving (Eq, Show)

data Client
  = Client
  { _size :: (Int,Int)
  , _focusing :: FocusOn
  , _timeline :: W.List T.Text CardId
  , _notification :: W.List T.Text CardId
  , _minibuffer :: W.Editor T.Text T.Text
  , _textarea :: W.Editor T.Text T.Text
  , _plugins :: (Int, M.Map PluginId Plugin)
  , _cardpool :: M.Map CardId Card
  }

makeLenses ''Client

cardix :: CardId -> Getter Client Card
cardix n = to $ \c -> c ^. cardpool ^. to (M.! n)

selectedCard :: Getter Client Card
selectedCard = to $ \cli -> cli ^. cardix (cli ^. timeline ^. to W.listSelectedElement ^?! _Just ^. _2)

selectedPlugin :: Getter Client Plugin
selectedPlugin = to $ \cli -> cli ^. plugins ^. _2 ^?! ix (M.keys (cli ^. plugins ^. _2) ^?! ix (cli ^. plugins ^. _1))

app :: App Client Card T.Text
app = App
  renderer
  showFirstCursor
  evhandler
  return
  (\_ -> attrMap (fg Vty.white) colorscheme)

  where
    renderer cli = case cli^.focusing of
      st | st `elem` [Timeline, Notification, Minibuffer] -> return $ vBox
        [ W.renderList (\b n -> renderCardWithIn (cli ^. size ^. _1) b (cli ^. cardix n)) (cli^.focusing == Timeline) (if (cli^.focusing == Notification) then cli ^. notification else cli ^. timeline)
        , withAttr "inverted" $ padRight Max $ txt $ "--- *" `T.append` T.pack (show $ cli ^. focusing) `T.append` "* [sys/tw/tm]"
        , W.renderEditor (cli^.focusing == Minibuffer) (cli ^. minibuffer)
        ]
      Item -> return $ vBox
        [ let w = cli^.size^._1^.to fromIntegral^.to (* 0.7)^.to floor in
          translateBy (Location (0,4)) $ W.hCenterLayer $ W.border $ padLeftRight 1 $ hLimit w $ renderDetailCardWithIn w (cli^.focusing == Timeline) (cli ^. selectedCard)
        ]
      Compose -> return $ vBox
        [ vLimit (cli ^. size ^. _2 - 6) $ W.renderList (\b n -> renderCardWithIn (cli ^. size ^. _1) b $ (cli ^. cardix n)) False (cli ^. timeline)
        , withAttr "inverted" $ padRight Max $ txt $ "--- [" `T.append` (cli ^. selectedPlugin ^. to pluginId ^. to textPluginId) `T.append` "] *textarea* (C-c)send (C-q)quit (C-n)switch"
        , vLimit 5 $ W.renderEditor (cli^.focusing == Compose) (cli ^. textarea)
        ]
      ReplyTo tx -> return $ vBox
        [ vLimit (cli ^. size ^. _2 - 7) $ W.renderList (\b n -> renderCardWithIn (cli ^. size ^. _1) b $ (cli ^. cardix n)) False (cli ^. timeline)
        , withAttr "inverted" $ padRight Max $ txt $ "--- [" `T.append` (cli ^. selectedPlugin ^. to pluginId ^. to textPluginId) `T.append` "] *reply* (C-c)send (C-q)quit (C-n)switch"
        , withAttr "inverted" $ padRight Max $ txt tx
        , vLimit 5 $ W.renderEditor True (cli ^. textarea)
        ]

    evhandler cli = \case
      VtyEvent evkey -> case cli^.focusing of
        Timeline -> case evkey of
          Vty.EvKey (Vty.KChar 'q') [] -> halt cli
          Vty.EvKey (Vty.KChar 'x') [Vty.MMeta] -> continue $ cli & focusing .~ Minibuffer
          Vty.EvKey (Vty.KChar 'a') [Vty.MCtrl] -> continue $ cli & focusing .~ Compose
          Vty.EvKey (Vty.KChar 'a') [Vty.MMeta] -> do
            let selplugin = cli^.plugins^._2^?!ix (cli^.selectedCard^.cardId^._CardId^._1)
            case selplugin^.to (flip replyTo (cli^.selectedCard)) of
              Just rinfo -> continue $ cli
                & textarea . W.editContentsL .~ textZipper [placeholder rinfo] Nothing
                & focusing .~ ReplyTo (description rinfo)
                & plugins . _1 .~ M.findIndex (selplugin ^. to pluginId) (cli ^. plugins ^. _2)
              Nothing -> continue cli
          Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl] -> continue $ cli & focusing .~ Notification
          Vty.EvKey (Vty.KChar 'i') [] -> continue $ cli & focusing .~ Item
          _ -> handleEventLensed cli timeline W.handleListEvent evkey >>= continue
        Minibuffer -> case evkey of
          Vty.EvKey (Vty.KChar 'g') [Vty.MCtrl] -> continue $ cli & focusing .~ Timeline
          _ -> handleEventLensed cli minibuffer W.handleEditorEvent evkey >>= continue
        Item -> case evkey of
          Vty.EvKey (Vty.KChar 'i') [] -> continue $ cli & focusing .~ Timeline
          Vty.EvKey (Vty.KChar ch) [] | ch `M.member` krmap -> liftIO (krmap M.! ch $ curcard) >> continue cli
            where
              curcard = cli^.cardix (cli^.timeline^.to W.listSelectedElement^?!_Just^._2)
              krmap = cli ^. plugins ^. _2 ^?! ix (curcard^.cardId^._CardId^._1) ^. to keyRunner
          _ -> continue cli
        Compose -> case evkey of
          Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl] -> continue $ cli & focusing .~ Timeline
          Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> do
            liftIO (cli^.selectedPlugin^.to (flip updater (cli^.textarea^.to W.getEditContents)))
            continue $ cli & focusing .~ Timeline & textarea . W.editContentsL %~ clearZipper
          Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl] | cli^.plugins^._2^.to length /= 0 -> continue $ cli & plugins . _1 %~ (\x -> (x+1) `mod` (length $ cli^.plugins^._2))
          _ -> handleEventLensed cli textarea W.handleEditorEvent evkey >>= continue
        ReplyTo _ -> case evkey of
          Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl] -> continue $ cli & focusing .~ Timeline
          Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> do
            let sel = cli^.cardix (cli^.timeline^.to W.listSelectedElement^?!_Just^._2)
            let Just rinfo = cli^.selectedPlugin^.to (flip replyTo sel)
            liftIO $ replyUpdater rinfo (cli^.textarea^.to W.getEditContents)
            continue $ cli & focusing .~ Timeline & textarea . W.editContentsL %~ clearZipper
          _ -> handleEventLensed cli textarea W.handleEditorEvent evkey >>= continue
        Notification -> case evkey of
          Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl] -> continue $ cli & focusing .~ Timeline
          _ -> handleEventLensed cli notification W.handleListEvent evkey >>= continue
      AppEvent card -> do
        continue $ cli &~ do
          cardpool %= M.insert (card^.cardId) card
          timeline %= W.listInsert (cli ^. timeline ^. W.listElementsL ^. to length - 1) (card ^. cardId)

          when ("notify" `elem` card ^. label) $ do
            notification %= W.listInsert (cli ^. notification ^. W.listElementsL ^. to length) (card ^. cardId)
            notification %= W.listMoveDown

          let Just (_,sel) = cli ^. timeline ^. to W.listSelectedElement
          when (cli ^. cardix sel ^. cardId == sentinel ^. cardId) $ timeline %= W.listMoveDown
      _ -> continue cli

    colorscheme =
      [ ("plugin-id", fg Vty.brightBlue)
      , ("user-name", Vty.withStyle Vty.currentAttr Vty.bold)
      , ("screen-name", fg Vty.red)
      , (attrName "inverted" <> attrName "card-content", Vty.black `on` Vty.white)
      , ("inverted", Vty.black `on` Vty.white)
      ]

sentinel :: Card
sentinel = Card (CardId (PluginId "sys" "sentinel") "*") "" "sentinel" "--- fetching new cards ---" Nothing []

defClient :: (Int,Int) -> Client
defClient s =
  Client
    s
    Timeline
    (W.list "timeline" V.empty 2)
    (W.list "notification" V.empty 2)
    (W.editorText "minibuffer" (txt . T.unlines) (Just 1) "")
    (W.editorText "textarea" (txt . T.unlines) (Just 5) "")
    (0,M.empty)
    M.empty

runClient :: [Plugin] -> IO ()
runClient pls = do
  size <- Vty.displayBounds =<< Vty.outputForConfig =<< Vty.standardIOConfig
  chan <- newBChan 2
  forM_ pls $ \p -> forkIO $ fetcher p chan

  customMain
    (Vty.standardIOConfig >>= Vty.mkVty)
    (Just chan)
    app
    $ defClient size
    & plugins .~ (0, M.fromList $ fmap (\p -> (p^.to pluginId,p)) pls)
    & cardpool %~ M.insert (sentinel^.cardId) sentinel
    & timeline %~ W.listInsert 0 (sentinel^.cardId)

  return ()

