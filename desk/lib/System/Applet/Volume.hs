{-# LANGUAGE OverloadedLabels #-}

module System.Applet.Volume where

import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.GI.Base.Signals
import Data.Text qualified as T
import GI.Gtk.Objects.Image qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Task qualified as Gtk
import Status.AudioStatus
import qualified Gtk.Containers as Gtk
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

-- TODO Add actions to change mute/volume

-- | Volume display. Currently only looks into "default:Master".
volumeDisplay :: MonadIO m => Gtk.IconSize -> m Gtk.Widget
volumeDisplay iconSize = do
  widIcon <- startRegular 100 volTask >>= traverse volIcon
  btn <- Gtk.buttonNewWith widIcon $ safeSpawn "pavucontrol" []
  #setName btn (T.pack "vol")
  btn <$ #showAll btn
  where
    volTask = curVolStat "default" "Master" >>= maybe (fail "cannot find") pure

    volIcon :: MonadIO m => Task VolStat -> m Gtk.Widget
    volIcon task = do
      vol <- new Gtk.Image [#iconSize := (fromIntegral . fromEnum) iconSize]
      liftIO $ do
        kill <- Gtk.uiTask task $ \stat -> do
          set vol [#iconName := volIconName (volLevel stat)]
        on vol #destroy kill
      Gtk.toWidget vol
