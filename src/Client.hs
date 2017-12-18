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
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Text.Zipper (clearZipper)
import Data.Unique

import Types

data FocusOn = Timeline | Notification | Minibuffer | Textarea | Item
  deriving (Eq, Show)

data Client
  = Client
  { _size :: (Int,Int)
  , _focusing :: FocusOn
  , _timeline :: W.List T.Text Int
  , _notification :: W.List T.Text Int
  , _minibuffer :: W.Editor T.Text T.Text
  , _textarea :: W.Editor T.Text T.Text
  , _plugins :: (Int, [Plugin])
  , _cardpool :: IM.IntMap Card
  }

makeLenses ''Client

app :: App Client Card T.Text
app = App
  renderer
  showFirstCursor
  evhandler
  return
  (\_ -> attrMap (fg Vty.white) colorscheme)

  where
    renderer cli | cli^.focusing `elem` [Timeline, Notification, Minibuffer] = return $ vBox
      [ W.renderList (\b n -> renderCardWithIn (cli ^. size ^. _1) b (cli ^. cardix n)) (cli^.focusing == Timeline) (if (cli^.focusing == Notification) then cli ^. notification else cli ^. timeline)
      , withAttr "inverted" $ padRight Max $ txt $ "--- *" `T.append` T.pack (show $ cli ^. focusing) `T.append` "* [sys/tw/tm]"
      , W.renderEditor (cli^.focusing == Minibuffer) (cli ^. minibuffer)
      ]
    renderer cli | cli^.focusing == Item = return $ vBox
      [ let w = cli^.size^._1^.to fromIntegral^.to (* 0.7)^.to floor in
        translateBy (Location (0,4)) $ W.hCenterLayer $ W.border $ padLeftRight 1 $ hLimit w $ renderDetailCardWithIn w (cli^.focusing == Timeline) (cli ^. cardix (cli^.timeline^.to W.listSelectedElement^?!_Just^._2))
      ]
    renderer cli | cli^.focusing == Textarea = return $ vBox
      [ vLimit (cli ^. size ^. _2 - 6) $ W.renderList (\b n -> renderCardWithIn (cli ^. size ^. _1) b $ (cli ^. cardix n)) (cli^.focusing == Timeline) (cli ^. timeline)
      , withAttr "inverted" $ padRight Max $ txt $ "--- [" `T.append` (cli ^. plugins ^. _2 ^? ix (cli ^. plugins ^. _1) ^. _Just . to pluginId) `T.append` "] *textarea* (C-c)send (C-q)quit (C-n)switch"
      , vLimit 5 $ W.renderEditor (cli^.focusing == Textarea) (cli ^. textarea)
      ]

    evhandler cli = \case
      VtyEvent evkey -> case cli^.focusing of
        Timeline -> case evkey of
          Vty.EvKey (Vty.KChar 'q') [] -> halt cli
          Vty.EvKey (Vty.KChar 'x') [Vty.MMeta] -> continue $ cli & focusing .~ Minibuffer
          Vty.EvKey (Vty.KChar 'a') [Vty.MCtrl] -> continue $ cli & focusing .~ Textarea
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
              krmap = cli ^. plugins ^. _2 ^. to (filter (\t -> t^.to pluginId == curcard^.pluginOf)) ^. to head ^. to keyRunner
          _ -> continue cli
        Textarea -> case evkey of
          Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl] -> continue $ cli & focusing .~ Timeline
          Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> do
            liftIO (cli^.plugins^._2^?ix (cli^.plugins^._1)^._Just.to (flip updater (cli^.textarea^.to W.getEditContents)))
            continue $ cli & focusing .~ Timeline & textarea . W.editContentsL %~ clearZipper
          Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl] | cli^.plugins^._2^.to length /= 0 -> continue $ cli & plugins . _1 %~ (\x -> (x+1) `mod` (length $ cli^.plugins^._2))
          _ -> handleEventLensed cli textarea W.handleEditorEvent evkey >>= continue
        Notification -> case evkey of
          Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl] -> continue $ cli & focusing .~ Timeline
          _ -> handleEventLensed cli notification W.handleListEvent evkey >>= continue
      AppEvent card -> do
        (continue =<<) $ newCardTL card cli <&> \(n,cli) -> cli &~ do
          when ("notify" `elem` card ^. label) $ do
            notification %= W.listInsert (cli ^. notification ^. W.listElementsL ^. to length) n
            notification %= W.listMoveDown

          let Just (_,sel) = cli ^. timeline ^. to W.listSelectedElement
          when (cli ^. cardix sel ^. pluginOf == sentinel ^. pluginOf) $ timeline %= W.listMoveDown
      _ -> continue cli

    colorscheme =
      [ ("plugin-id", fg Vty.brightBlue)
      , ("user-name", Vty.withStyle Vty.currentAttr Vty.bold)
      , ("screen-name", fg Vty.red)
      , (attrName "inverted" <> attrName "card-content", Vty.black `on` Vty.white)
      , ("inverted", Vty.black `on` Vty.white)
      ]

sentinel :: Card
sentinel = Card "sys/sentinel" "" "sentinel" "--- fetching new cards ---" Nothing []

defClient :: (Int,Int) -> Client
defClient s =
  Client
    s
    Timeline
    (W.list "timeline" V.empty 2)
    (W.list "notification" V.empty 2)
    (W.editorText "minibuffer" (txt . T.unlines) (Just 1) "")
    (W.editorText "textarea" (txt . T.unlines) (Just 5) "")
    (0,[])
    IM.empty

runClient :: [Plugin] -> IO ()
runClient pls = do
  size <- Vty.displayBounds =<< Vty.outputForConfig =<< Vty.standardIOConfig
  chan <- newBChan 2
  forM_ pls $ \p -> do
    forkIO $ fetcher p chan
    putStrLn $ T.unpack $ "[" `T.append` pluginId p `T.append` "] booted"

  customMain
    (Vty.standardIOConfig >>= Vty.mkVty)
    (Just chan)
    app
    =<< (defClient size & plugins .~ (0,pls) & newCardTL_ sentinel)

  return ()

newCardTL :: MonadIO m => Card -> Client -> m (Int,Client)
newCardTL card cli = liftIO $ do
  n <- hashUnique <$> newUnique
  return $ (,) n $ cli & cardpool %~ IM.insert n card & timeline %~ W.listInsert (cli ^. timeline ^. W.listElementsL ^. to length - 1) n

newCardTL_ :: MonadIO m => Card -> Client -> m Client
newCardTL_ card cli = snd <$> newCardTL card cli

cardix :: Int -> Getter Client Card
cardix n = to $ \c -> c ^. cardpool ^. to (IM.! n)

