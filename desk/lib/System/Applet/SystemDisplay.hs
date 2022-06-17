module System.Applet.SystemDisplay where

import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.Foldable
import Data.Int
import Data.Ratio ((%))
import Data.Text qualified as T
import GI.Gtk.Objects.Image qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
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

-- | Mainboard status display with given icon size & width.
mainboardDisplay :: MonadIO m => Gtk.IconSize -> Int32 -> m Gtk.Widget
mainboardDisplay iconSize mainWidth = do
  widMem <- startRegular 500 memStat >>= traverse memIcon
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
    memIcon task = do
      -- TODO Images to represent memory temp changes
      -- TODO Identify the transparency issue of the image
      -- MAYBE Image itself providing location of bar?
      let ramImg = View.imageStaticNew iconSize True $ View.ImgSName (T.pack "ram-symbolic")
      hack <- ramImg
      fg <- ramImg
      let barRect = RationalRect (14 % 32) (8 % 32) (18 % 32) (24 % 32)
      bar <- View.barNew Gtk.OrientationVertical barRect
      mem <- Gtk.overlayed hack [View.barWidget bar, fg]
      Gtk.widgetSetName mem (T.pack "mem")
      liftIO $ do
        kill <- Gtk.uiTask task $ View.barSetFill bar . memUsed . memRatios
        Gtk.onWidgetDestroy mem kill
      pure mem

    cpuIcon (taskUse, taskTemp) = do
      fg <- View.imageStaticNew iconSize True $ View.ImgSName (T.pack "cpu-symbolic")
      let barRect = RationalRect (28 % 64) (25 % 64) (36 % 64) (39 % 64)
      bar <- View.barNew Gtk.OrientationVertical barRect
      cpu <- Gtk.overlayed fg [View.barWidget bar]
      Gtk.widgetSetName cpu (T.pack "cpu")
      liftIO $ do
        killUse <- Gtk.uiTask taskUse $ View.barSetFill bar . cpuUsed . cpuRatios
        killTm <- Gtk.uiTask taskTemp $ updateTemp fg . tempVal
        Gtk.onWidgetDestroy cpu (killUse >> killTm)
      pure cpu

    updateTemp fg tempV = Gtk.widgetGetStyleContext fg >>= Gtk.updateCssClass tempClass [tempV]

