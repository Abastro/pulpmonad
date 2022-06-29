module System.Applet.SystemDisplay where

import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.Foldable
import Data.GI.Base qualified as Glib
import Data.Int
import Data.Ratio ((%))
import Data.Text qualified as T
import GI.Gtk.Objects.Image qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.ImageBar qualified as Gtk
import Gtk.Styles qualified as Gtk
import Gtk.Task qualified as Gtk
import Status.HWStatus
import Text.Printf
import View.Imagery qualified as View
import XMonad.StackSet (RationalRect (..))
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
  Gtk.widgetSetName ev (T.pack "bat")
  ev <$ Gtk.widgetShowAll ev
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

-- MAYBE Image itself providing location of bar?

-- | Mainboard status display with given icon size & width.
mainboardDisplay :: MonadIO m => Gtk.IconSize -> Int32 -> m Gtk.Widget
mainboardDisplay iconSize mainWidth = do
  widMem <- startRegular 500 memStat >>= traverse testIcon
  traverse_ (`Gtk.setWidgetHalign` Gtk.AlignStart) widMem
  traverse_ (`Gtk.setWidgetValign` Gtk.AlignCenter) widMem

  widCPU <- do
    cpuUse <- startRegular 50 (cpuDelta 50)
    cpuTemp <- startRegular 100 cpuTemp
    traverse cpuIcon ((,) <$> cpuUse <*> cpuTemp)
  traverse_ (`Gtk.setWidgetHalign` Gtk.AlignEnd) widCPU
  traverse_ (`Gtk.setWidgetValign` Gtk.AlignCenter) widCPU

  bg <- do
    img <- Gtk.imageNew -- Tried to set the image, but gtk does not accept rectangular icons.
    Gtk.widgetSetSizeRequest img mainWidth (-1)
    Gtk.toWidget img

  disp <- Gtk.overlayed bg (toList widMem <> toList widCPU)
  ev <- Gtk.buttonNewWith (Just disp) $ safeSpawn "gnome-system-monitor" ["-r"]
  Gtk.widgetSetName ev (T.pack "mainboard")
  ev <$ Gtk.widgetShowAll ev
  where
    testIcon task = do
      mem <- Glib.new Gtk.ImageBar []
      let barRect = RationalRect (14 % 32) (8 % 32) (4 % 32) (16 % 32)
      Gtk.widgetSetName mem (T.pack "mem")
      liftIO $ do
        Gtk.imageBarPos mem Gtk.OrientationVertical barRect
        Gtk.imageBarSetIcon mem (T.pack "ram-symbolic") iconSize
        kill <- Gtk.uiTask task $ Gtk.imageBarSetFill mem . memUsed . memRatios
        Gtk.onWidgetDestroy mem kill
      Gtk.toWidget mem

    cpuIcon (taskUse, taskTemp) = do
      cpu <- Glib.new Gtk.ImageBar []
      let barRect = RationalRect (28 % 64) (25 % 64) (8 % 64) (14 % 64)
      Gtk.widgetSetName cpu (T.pack "cpu")
      liftIO $ do
        Gtk.imageBarPos cpu Gtk.OrientationVertical barRect
        Gtk.imageBarSetIcon cpu (T.pack "cpu-symbolic") iconSize
        killUse <- Gtk.uiTask taskUse $ Gtk.imageBarSetFill cpu . cpuUsed . cpuRatios
        killTm <- Gtk.uiTask taskTemp $ updateTemp cpu . tempVal
        Gtk.onWidgetDestroy cpu (killUse >> killTm)
      Gtk.toWidget cpu

    updateTemp fg tempV = Gtk.widgetGetStyleContext fg >>= Gtk.updateCssClass tempClass [tempV]
