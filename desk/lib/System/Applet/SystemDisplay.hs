{-# LANGUAGE OverloadedLabels #-}

module System.Applet.SystemDisplay (
  batDisplay
  , mainboardDisplay
) where

import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.Foldable
import Data.GI.Base.Constructible
import Data.GI.Base.Signals
import Data.Int
import Data.Text qualified as T
import GI.Gtk.Objects.Image qualified as Gtk
import Graphics.X11.Xlib.Types (Rectangle (..))
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.ImageBar qualified as Gtk
import Gtk.Styles qualified as Gtk
import Gtk.Task qualified as Gtk
import Status.HWStatus
import Text.Printf
import View.Imagery qualified as View
import XMonad.Util.Run (safeSpawn)

data Temperature = T20 | T40 | T60 | T80 | T100 | T120
  deriving (Eq, Ord, Enum, Bounded)

tempVal :: Double -> Temperature
tempVal temp = toEnum . round $ (max 19 (min 121 temp) - 20) / 20

tempClass :: Temperature -> T.Text
tempClass = \case
  T20 -> T.pack "temp-20"
  T40 -> T.pack "temp-40"
  T60 -> T.pack "temp-60"
  T80 -> T.pack "temp-80"
  T100 -> T.pack "temp-100"
  T120 -> T.pack "temp-120"

-- TODO Warning colors when too full

-- | Battery status display.
batDisplay :: MonadIO m => Gtk.IconSize -> m Gtk.Widget
batDisplay iconSize = do
  img <- startRegular 500 batStat >>= traverse batIcon
  ev <- Gtk.buttonNewWith (View.imageDynWidget <$> img) $ safeSpawn "gnome-control-center" ["power"]
  #setName ev (T.pack "bat")
  ev <$ #showAll ev
  where
    batIcon task = do
      img <- View.imageDynNew iconSize True
      liftIO $ do
        kill <- Gtk.uiTask task $ \BatStat{capacity, batStatus} ->
          View.imageDynSetImg img $ View.ImgSName $ batName ((capacity `div` 10) * 10) batStatus
        Gtk.onWidgetDestroy (View.imageDynWidget img) kill
      pure img

    batName level = \case
      Charging -> T.pack $ printf "battery-level-%d-charging-symbolic" level
      _ -> T.pack $ printf "battery-level-%d-symbolic" level

-- MAYBE Migrate to GtkBuilder.
-- `g_type_ensure` would likely ensure it is loaded.
-- Calling `glibType` could be enough instead.

-- MAYBE Roll down "menu" showing status & settings, on hold until above is resolved

-- | Mainboard status display with given icon size & width.
mainboardDisplay :: MonadIO m => Gtk.IconSize -> Int32 -> m Gtk.Widget
mainboardDisplay iconSize mainWidth = do
  widMem <- startRegular 500 memStat >>= traverse testIcon
  traverse_ (flip #setHalign Gtk.AlignStart) widMem
  traverse_ (flip #setValign Gtk.AlignCenter) widMem

  widCPU <- do
    cpuUse <- startRegular 50 (cpuDelta 50)
    cpuTemp <- startRegular 100 cpuTemp
    traverse cpuIcon ((,) <$> cpuUse <*> cpuTemp)
  traverse_ (flip #setHalign Gtk.AlignEnd) widCPU
  traverse_ (flip #setValign Gtk.AlignCenter) widCPU

  -- Coping with Gtk space allocation
  bg <- do
    img <- new Gtk.Image [] -- Tried to set an icon, but gtk does not accept rectangular icons.
    #setSizeRequest img mainWidth (-1)
    Gtk.toWidget img

  disp <- Gtk.overlayed bg (toList widMem <> toList widCPU)
  ev <- Gtk.buttonNewWith (Just disp) $ safeSpawn "gnome-system-monitor" ["-r"]
  #setName ev (T.pack "mainboard")
  ev <$ #showAll ev
  where
    testIcon task = do
      mem <- new Gtk.ImageBar []
      #setName mem (T.pack "mem")
      liftIO $ do
        Gtk.imageBarPos mem Gtk.OrientationVertical 32 (Rectangle 14 8 4 16)
        Gtk.imageBarSetIcon mem (T.pack "ram-symbolic") iconSize
        kill <- Gtk.uiTask task $ Gtk.imageBarSetFill mem . memUsed . memRatios
        on mem #destroy kill
      Gtk.toWidget mem

    cpuIcon (taskUse, taskTemp) = do
      cpu <- new Gtk.ImageBar []
      #setName cpu (T.pack "cpu")
      liftIO $ do
        Gtk.imageBarPos cpu Gtk.OrientationVertical 64 (Rectangle 28 25 8 14)
        Gtk.imageBarSetIcon cpu (T.pack "cpu-symbolic") iconSize
        killUse <- Gtk.uiTask taskUse $ Gtk.imageBarSetFill cpu . cpuUsed . cpuRatios
        killTm <- Gtk.uiTask taskTemp $ updateTemp cpu . tempVal
        on cpu #destroy (killUse >> killTm)
      Gtk.toWidget cpu

    updateTemp fg tempV = #getStyleContext fg >>= Gtk.updateCssClass tempClass [tempV]
