{-# LANGUAGE OverloadedLabels #-}

module System.Applet.Volume (volumeDisplay) where

import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.GI.Base.Signals
import Data.Ord
import Data.Text qualified as T
import GI.Gdk.Structs.EventScroll qualified as Gdk
import GI.Gtk.Objects.Image qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.Task qualified as Gtk
import Status.AudioStatus
import XMonad.Util.Run

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

-- TODO Add actions to change mute state

-- | Volume display. First argument is mixer name, second is control name.
volumeDisplay :: MonadIO m => String -> String -> Gtk.IconSize -> m Gtk.Widget
volumeDisplay mixerName controlName iconSize = do
  widIcon <- startRegular 100 getVolume >>= traverse volIcon
  btn <- Gtk.buttonNewWith widIcon $ safeSpawn "pavucontrol" []
  #setName btn (T.pack "vol")
  #addEvents btn [Gtk.EventMaskScrollMask]
  on btn #scrollEvent onScroll
  btn <$ #showAll btn
  where
    getVolume = curVolStat mixerName controlName >>= maybe (fail "cannot find") pure

    chVolume adj = updateRelVolume mixerName controlName (\vol -> clamp (0, 1) $ vol + adj)

    volIcon :: MonadIO m => Task VolStat -> m Gtk.Widget
    volIcon task = do
      vol <- new Gtk.Image [#iconSize := (fromIntegral . fromEnum) iconSize]
      liftIO $ do
        kill <- Gtk.uiTask task $ \stat -> do
          set vol [#iconName := volIconName (volLevel stat)]
        on vol #destroy kill
      Gtk.toWidget vol

    onScroll :: Gdk.EventScroll -> IO Bool
    onScroll event = do
      get event #direction >>= \case
        Gtk.ScrollDirectionUp -> True <$ chVolume (1 / 100)
        Gtk.ScrollDirectionDown -> True <$ chVolume (-1 / 100)
        _ -> pure False

{-------------------------------------------------------------------
                              View
--------------------------------------------------------------------}

{-
data View = View
  { volWidget :: Gtk.Widget
  , clicks :: Source ()
  , scrolls :: Source Gdk.EventScroll
  }
  -}
