{-# LANGUAGE OverloadedLabels #-}

module System.Applet.SystemDisplay (
  batDisplay,
  mainboardDisplay,
) where

import Control.Concurrent.Task
import Control.Event.Entry
import Control.Monad.IO.Class
import Data.GI.Base.Attributes
import Data.GI.Base.BasicTypes
import Data.GI.Base.Constructible
import Data.GI.Base.Signals
import Data.Int
import Data.Text qualified as T
import GI.Gtk.Objects.Image qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.ImageBar qualified as Gtk
import Gtk.Styles qualified as Gtk
import Gtk.Task qualified as Gtk
import Status.HWStatus
import System.FilePath
import System.Pulp.PulpEnv
import Text.Printf
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

-- | Battery status display.
batDisplay :: (MonadIO m, MonadPulpPath m) => Gtk.IconSize -> m Gtk.Widget
batDisplay _iconSize =
  startRegular 500 batStat >>= \case
    Nothing -> Gtk.toWidget =<< new Gtk.Image [] -- Empty image when battery cannot be accessed
    Just batt -> do
      uiFile <- pulpDataPath ("ui" </> "battery.ui")
      BatView{..} <- liftIO $ batView (T.pack uiFile) $ safeSpawn "gnome-control-center" ["power"]
      liftIO $ do
        kill <- Gtk.uiTask batt $ \BatStat{capacity, batStatus} ->
          setBatIcon $ batName ((capacity `div` 10) * 10) batStatus
        Gtk.onWidgetDestroy batWidget kill
      pure batWidget
  where
    batName level = \case
      Charging -> T.pack $ printf "battery-level-%d-charging-symbolic" level
      _ -> T.pack $ printf "battery-level-%d-symbolic" level

data BatView = BatView
  { batWidget :: !Gtk.Widget
  , setBatIcon :: Sink T.Text
  }

batView :: T.Text -> IO () -> IO BatView
batView uiFile act = Gtk.buildFromFile uiFile $ do
  Just batWidget <- Gtk.getElement (T.pack "battery") Gtk.Widget
  Just batIcon <- Gtk.getElement (T.pack "battery-icon") Gtk.Image

  let setBatIcon icon = set batIcon [#iconName := icon]
  Gtk.addCallback (T.pack "battery-open") act
  pure BatView{..}

-- MAYBE Roll down "menu" showing status & settings, on hold until above is resolved

-- TODO Warning colors when too full

-- | Mainboard status display with given icon size & width.
mainboardDisplay :: (MonadIO m, MonadPulpPath m) => Gtk.IconSize -> Int32 -> m Gtk.Widget
mainboardDisplay _iconSize _mainWidth = do
  memUse <- startRegular 500 memStat
  cpuUse <- startRegular 50 (cpuDelta 50)
  cpuTemps <- startRegular 100 cpuTemp
  case (,,) <$> memUse <*> cpuUse <*> cpuTemps of
    Nothing -> Gtk.toWidget =<< new Gtk.Image []
    Just (memUse, cpuUse, cpuTemps) -> do
      uiFile <- pulpDataPath ("ui" </> "mainboard.ui")
      MainboardView{..} <- liftIO $ mainboardView (T.pack uiFile) $ safeSpawn "gnome-system-monitor" ["-r"]
      liftIO $ do
        killMem <- Gtk.uiTask memUse $ setMemFill . memUsed . memRatios
        killCUse <- Gtk.uiTask cpuUse $ setCPUFill . cpuUsed . cpuRatios
        killCTemp <- Gtk.uiTask cpuTemps $ setCPUTemp . tempVal
        on mainboardWidget #destroy (killMem *> killCUse *> killCTemp)
      pure mainboardWidget

data MainboardView = MainboardView
  { mainboardWidget :: !Gtk.Widget
  , setMemFill :: Sink Double
  , setCPUFill :: Sink Double
  , setCPUTemp :: Sink Temperature
  }

mainboardView :: T.Text -> IO () -> IO MainboardView
mainboardView uiFile act = do
  glibType @Gtk.ImageBar -- Ensures ImageBar is registered
  Gtk.buildFromFile uiFile $ do
    Just mainboardWidget <- Gtk.getElement (T.pack "mainboard") Gtk.Widget
    Just memBar <- Gtk.getElement (T.pack "mainboard-mem") Gtk.ImageBar
    Just cpuBar <- Gtk.getElement (T.pack "mainboard-cpu") Gtk.ImageBar

    let setMemFill = Gtk.imageBarSetFill memBar
        setCPUFill = Gtk.imageBarSetFill cpuBar
        setCPUTemp tempV = #getStyleContext cpuBar >>= Gtk.updateCssClass tempClass [tempV]

    Gtk.addCallback (T.pack "mainboard-open") act
    pure MainboardView{..}
