module System.Pulp.Applet.SystemDisplay where

import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.Foldable
import Data.Ratio ((%))
import Data.Text qualified as T
import Status.HWStatus
import Text.Printf
import UI.Commons qualified as UI
import UI.Containers qualified as UI
import UI.Singles qualified as UI
import UI.Task qualified as UI
import View.Imagery qualified as View
import XMonad.StackSet (RationalRect (..))
import XMonad.Util.Run (safeSpawn)

-- | Battery status display.
batDisplay :: MonadIO m => m UI.Widget
batDisplay = do
  img <- startRegular 500 batStat >>= traverse batIcon
  ev <- UI.buttonNewWith (View.imageDynWidget <$> img) $ safeSpawn "gnome-control-center" ["power"]
  UI.widgetSetName ev (T.pack "bat")
  ev <$ UI.widgetShowAll ev
  where
    batIcon task = do
      img <- View.imageDynNew UI.IconSizeDnd
      liftIO $ do
        kill <- UI.uiTask task $ \BatStat{capacity, batStatus} ->
          View.imageDynSetImg img $ View.ImgSName $ batName ((capacity `div` 10) * 10) batStatus
        UI.onWidgetDestroy (View.imageDynWidget img) kill
      pure img

    batName level = \case
      Charging -> T.pack $ printf "battery-level-%d-charging-symbolic" level
      _ -> T.pack $ printf "battery-level-%d-symbolic" level

-- | Mainboard status display.
mainboardDisplay :: MonadIO m => m UI.Widget
mainboardDisplay = do
  widMem <- startRegular 500 memStat >>= traverse memIcon
  traverse_ (`UI.setWidgetHalign` UI.AlignStart) widMem
  traverse_ (`UI.setWidgetValign` UI.AlignCenter) widMem

  widCPU <- do
    cpuUse <- startRegular 50 (cpuDelta 50)
    cpuTemp <- startRegular 100 cpuTemp
    traverse cpuIcon ((,) <$> cpuUse <*> cpuTemp)
  traverse_ (`UI.setWidgetHalign` UI.AlignEnd) widCPU
  traverse_ (`UI.setWidgetValign` UI.AlignCenter) widCPU

  bg <- do
    img <- UI.imageNew -- Will set image later
    UI.widgetSetSizeRequest img 56 32
    UI.toWidget img

  disp <- UI.overlayed bg (toList widMem <> toList widCPU)
  ev <- UI.buttonNewWith (Just disp) $ safeSpawn "gnome-system-monitor" ["-r"]
  ev <$ UI.widgetShowAll ev
  where
    memIcon task = do
      -- TODO Identify the transparency issue of the image
      -- MAYBE Image itself providing location of bar?
      let ramImg = View.imageStaticNew UI.IconSizeDnd $ View.ImgSName (T.pack "ram-000")
      hack <- ramImg
      fg <- ramImg
      let barRect = RationalRect (14 % 32) (8 % 32) (18 % 32) (24 % 32)
      bar <- View.barNew barRect
      mem <- UI.overlayed hack [View.barWidget bar, fg]
      UI.widgetSetName mem (T.pack "mem")
      liftIO $ do
        kill <- UI.uiTask task $ View.barSetFill bar . memUsed . memRatios
        UI.onWidgetDestroy mem kill
      pure mem

    cpuIcon (taskUse, taskTemp) = do
      fg <- View.imageDynNew UI.IconSizeDnd
      let barRect = RationalRect (28 % 64) (25 % 64) (36 % 64) (39 % 64)
      bar <- View.barNew barRect
      cpu <- UI.overlayed (View.imageDynWidget fg) [View.barWidget bar]
      UI.widgetSetName cpu (T.pack "cpu")
      liftIO $ do
        killUse <- UI.uiTask taskUse $ View.barSetFill bar . cpuUsed . cpuRatios
        killTm <- UI.uiTask taskTemp $ View.imageDynSetImg fg . View.ImgSName . cpuN . tmpInd
        UI.onWidgetDestroy cpu (killUse >> killTm)
      pure cpu

    tmpInd temp = round $ (max 0 . min 100 $ temp - 20) * 0.05
    cpuN (n :: Int) = T.pack $ printf "cpu-%03d" (n * 20)
