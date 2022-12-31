{-# LANGUAGE OverloadedLabels #-}

module System.Applet.Volume (volumeDisplay) where

import Control.Event.Entry
import Control.Monad.IO.Class
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.Ord
import Data.Text qualified as T
import GI.Gdk.Structs.EventScroll qualified as Gdk
import GI.Gdk.Unions.Event qualified as Gdk
import GI.Gtk.Objects.Image qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Task qualified as Gtk
import Reactive.Banana.Frameworks
import Status.AudioStatus
import System.FilePath
import System.Pulp.PulpEnv
import XMonad.Util.Run
import Control.Monad
import Reactive.Banana.Combinators
import qualified GI.Gdk.Structs.EventButton as Gdk

data VolumeLevel = Muted | VolLow | VolMid | VolHigh
  deriving (Eq, Ord, Enum, Bounded)

volLevel :: VolStat -> VolumeLevel
volLevel VolStat{..}
  | not isUnmuted || curVolume < 0.001 = Muted
  | curVolume < 1 / 3 = VolLow
  | curVolume < 2 / 3 = VolMid
  | otherwise = VolHigh

volIconName :: VolumeLevel -> T.Text
volIconName = \case
  Muted -> T.pack "audio-volume-muted-symbolic"
  VolLow -> T.pack "audio-volume-low-symbolic"
  VolMid -> T.pack "audio-volume-medium-symbolic"
  VolHigh -> T.pack "audio-volume-high-symbolic"

-- TODO Warn when "Nothing" is received

-- | Volume display. First argument is mixer name, second is control name.
volumeDisplay :: (MonadIO m, MonadPulpPath m) => String -> String -> m Gtk.Widget
volumeDisplay mixerName controlName = do
  uiFile <- pulpDataPath ("ui" </> "volume.ui")
  View{..} <- liftIO $ view (T.pack uiFile)

  let netDesc initVol = do
        ticker <- liftIO (periodicSource 200) >>= sourceEvent
        clickEvent <- sourceEvent clicks
        scrollEvent <- sourceEvent scrolls
        switchEvent <- sourceEvent switches

        -- Ticker and Scroll/Switch updates volume view
        volSamples <- mapEventIO (\() -> getVolume) (ticker <> void scrollEvent <> switchEvent)
        volumes <- stepper initVol (filterJust volSamples)
        syncBehavior volumes (Gtk.uiSingleRun . setVolIcon . volIconName . volLevel)
        reactimate (safeSpawnProg "pavucontrol" <$ clickEvent)
        reactimate (onScroll <$> scrollEvent)
        reactimate (onSwitch <$ switchEvent)

  liftIO getVolume >>= \case
    Nothing -> Gtk.toWidget =<< new Gtk.Image [] -- Empty image when volume cannot be accessed
    Just initVol -> do
      network <- liftIO . compile $ netDesc initVol
      liftIO $ actuate network
      pure volWidget
  where
    getVolume = curVolStat mixerName controlName
    chVolume adj = updateRelVolume mixerName controlName (\vol -> clamp (0, 1) $ vol + adj)

    onScroll Upward = chVolume (1 / 100)
    onScroll Downward = chVolume (-1 / 100)
    onSwitch = updateUnmuted mixerName controlName not

{-------------------------------------------------------------------
                              View
--------------------------------------------------------------------}

data ScrollTo = Upward | Downward

data View = View
  { volWidget :: !Gtk.Widget
  , setVolIcon :: Sink T.Text
  , clicks :: Source ()
  , scrolls :: Source ScrollTo
  , switches :: Source ()
  }

view :: T.Text -> IO View
view uiFile = Gtk.buildFromFile uiFile $ do
  Just volWidget <- Gtk.getElement (T.pack "volume") Gtk.Widget
  Just volImage <- Gtk.getElement (T.pack "image-volume") Gtk.Image
  -- Button does not receive scrolls, so it need to be manually enabled
  #addEvents volWidget [Gtk.EventMaskScrollMask]

  let setVolIcon icon = set volImage [#iconName := icon]

  (clicks, onClick) <- liftIO sourceSink
  (scrolls, onScroll) <- liftIO sourceSink
  (switches, onRelease) <- liftIO sourceSink
  Gtk.addCallback (T.pack "volume-open") $ onClick ()
  Gtk.addCallbackWithEvent (T.pack "volume-move") Gdk.getEventScroll $ handleScroll onScroll
  Gtk.addCallbackWithEvent (T.pack "volume-switch") Gdk.getEventButton $ handleRelease onRelease

  pure View{..}
  where
    handleScroll :: Sink ScrollTo -> Sink Gdk.EventScroll
    handleScroll callback event =
      get event #direction >>= \case
        Gtk.ScrollDirectionUp -> callback Upward
        Gtk.ScrollDirectionDown -> callback Downward
        _ -> pure ()

    handleRelease :: Sink () -> Sink Gdk.EventButton
    handleRelease callback event =
      get event #button >>= \case
        2 -> callback ()
        _ -> pure ()
