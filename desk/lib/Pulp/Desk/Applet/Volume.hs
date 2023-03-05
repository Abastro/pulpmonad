{-# LANGUAGE OverloadedLabels #-}

module Pulp.Desk.Applet.Volume (volumeDisplay) where

import Control.Monad
import Control.Monad.IO.Class
import Data.GI.Base.Attributes
import Data.Ord
import Data.Text qualified as T
import GI.Gdk.Structs.EventButton qualified as Gdk
import GI.Gdk.Structs.EventScroll qualified as Gdk
import GI.Gdk.Unions.Event qualified as Gdk
import GI.Gtk.Objects.Image qualified as Gtk
import Pulp.Desk.Env.PulpEnv
import Pulp.Desk.System.AudioStatus
import Pulp.Desk.UI.Commons qualified as Gtk
import Pulp.Desk.UI.Task qualified as Gtk
import Reactive.Banana.Frameworks
import XMonad.Util.Run
import Pulp.Desk.Reactive.Entry
import Pulp.Desk.PulpPath

data VolumeLevel = Muted | VolLow | VolMid | VolHigh
  deriving (Eq, Ord, Enum, Bounded)

volLevel :: VolStat -> VolumeLevel
volLevel VolStat{..}
  | not isUnmuted || curVolume < 0.001 = Muted
  | curVolume < 1 / 3 = VolLow
  | curVolume < 2 / 3 = VolMid
  | otherwise = VolHigh

volIconName :: Maybe VolumeLevel -> T.Text
volIconName = \case
  Just Muted -> T.pack "audio-volume-muted-symbolic"
  Just VolLow -> T.pack "audio-volume-low-symbolic"
  Just VolMid -> T.pack "audio-volume-medium-symbolic"
  Just VolHigh -> T.pack "audio-volume-high-symbolic"
  Nothing -> T.pack "volume-warning-symbolic"

-- TODO Warn when "Nothing" is received (proper logging)

-- | Volume display. First argument is mixer name, second is control name.
volumeDisplay :: String -> String -> PulpIO Gtk.Widget
volumeDisplay mixerName controlName = liftIO $ do
  uiFile <- dataPath ("ui" </> "volume.ui")
  View{..} <- view (T.pack uiFile)

  network <- compile $ do
    ticker <- sourceEvent (periodicSource 200)
    clickEvent <- sourceEvent clicks
    scrollEvent <- sourceEvent scrolls
    switchEvent <- sourceEvent switches

    volume <- pollingBehavior getVolume (ticker <> void scrollEvent <> switchEvent)
    syncBehavior volume (Gtk.uiSingleRun . setVolIcon . volIconName . fmap volLevel)
    reactimate (safeSpawnProg "pavucontrol" <$ clickEvent)
    reactimate (onScroll <$> scrollEvent)
    reactimate (onSwitch <$ switchEvent)
  actuate network

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
