{-# LANGUAGE OverloadedLabels #-}

module System.Applet.SystemDisplay (
  batDisplay,
  mainboardDisplay,
) where

import Control.Event.Entry
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.GI.Base.Attributes
import Data.GI.Base.BasicTypes
import Data.Int
import Data.Text qualified as T
import GI.Gtk.Objects.Image qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.ImageBar qualified as Gtk
import Gtk.Styles qualified as Gtk
import Gtk.Task qualified as Gtk
import Reactive.Banana.Frameworks
import Status.HWStatus
import System.FilePath
import System.Log.LogPrint
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

{-------------------------------------------------------------------
                            Battery
--------------------------------------------------------------------}

batDisplay :: (MonadUnliftIO m, MonadPulpPath m, MonadLog m) => Gtk.IconSize -> m Gtk.Widget
batDisplay _iconSize = withRunInIO $ \unlift -> do
  uiFile <- unlift $ pulpDataPath ("ui" </> "battery.ui")
  BatView{..} <- batView (T.pack uiFile)

  network <- compile $ do
    ticker <- liftIO (periodicSource 500) >>= sourceEvent
    clicks <- sourceEvent batClicks
    battery <- pollingBehavior getBattery ticker

    syncBehavior battery (Gtk.uiSingleRun . setBatIcon . iconOf)
    syncBehavior battery (unlift . handleExc)
    reactimate (safeSpawn "gnome-control-center" ["power"] <$ clicks)
  actuate network
  pure batWidget
  where
    getBattery = try @IOException batStat
    iconOf = \case
      Right BatStat{capacity, batStatus = Charging} ->
        T.pack $ printf "battery-level-%d-charging-symbolic" (levelOf capacity)
      Right BatStat{capacity, batStatus = _} ->
        T.pack $ printf "battery-level-%d-symbolic" (levelOf capacity)
      Left _ -> T.pack "battery-missing-symbolic"
    levelOf capacity = (capacity `div` 10) * 10

    handleExc (Left exc) = logS (T.pack "Battery") LevelWarn $ logStrf "Battery error : $1" (show exc)
    handleExc _ = pure ()

data BatView = BatView
  { batWidget :: !Gtk.Widget
  , batClicks :: Source ()
  , setBatIcon :: Sink T.Text
  }

batView :: T.Text -> IO BatView
batView uiFile = Gtk.buildFromFile uiFile $ do
  Just batWidget <- Gtk.getElement (T.pack "battery") Gtk.Widget
  Just batIcon <- Gtk.getElement (T.pack "battery-icon") Gtk.Image

  let setBatIcon icon = set batIcon [#iconName := icon]
  (batClicks, onClick) <- liftIO sourceSink
  Gtk.addCallback (T.pack "battery-open") $ onClick ()
  pure BatView{..}

{-------------------------------------------------------------------
                            Mainboard
--------------------------------------------------------------------}

-- MAYBE Roll down "menu" showing status & settings

-- TODO Warning colors when too full

mainboardDisplay :: (MonadUnliftIO m, MonadPulpPath m, MonadLog m) => Gtk.IconSize -> Int32 -> m Gtk.Widget
mainboardDisplay _iconSize _mainWidth = withRunInIO $ \unlift -> do
  uiFile <- unlift $ pulpDataPath ("ui" </> "mainboard.ui")
  MainboardView{..} <- mainboardView (T.pack uiFile)

  network <- compile $ do
    clicks <- sourceEvent mainClicks
    memTicker <- liftIO (periodicSource 500) >>= sourceEvent
    -- TODO Measure CPU Use properly by diffs every 100 second
    cpuUseTicker <- liftIO (periodicSource 50) >>= sourceEvent
    cpuTempTicker <- liftIO (periodicSource 100) >>= sourceEvent
    memory <- pollingBehavior getMemory memTicker
    cpuUse <- pollingBehavior getCPUUse cpuUseTicker
    cpuTemp <- pollingBehavior getCPUTemp cpuTempTicker

    -- TODO Set icon when data is missing
    syncBehavior memory (unlift . handleMem setMemFill)
    syncBehavior cpuUse (unlift . handleCPUUse setCPUFill)
    syncBehavior cpuTemp (unlift . handleCPUTemp setCPUTemp)
    reactimate (safeSpawn "gnome-system-monitor" ["-r"] <$ clicks)
  actuate network
  pure mainboardWidget
  where
    getMemory = try @IOException memStat
    getCPUUse = try @IOException $ cpuDelta 50
    getCPUTemp = try @IOException $ cpuTemp

    handleMem setFill = \case
      Right memory -> liftIO . Gtk.uiSingleRun . setFill $ memUsed (memRatios memory)
      Left exc -> logS (T.pack "Memory") LevelWarn $ logStrf "Memory error : $1" (show exc)
    handleCPUUse setFill = \case
      Right cpuUse -> liftIO . Gtk.uiSingleRun . setFill $ cpuUsed (cpuRatios cpuUse)
      Left exc -> logS (T.pack "CPU") LevelWarn $ logStrf "CPU error (usage) : $1" (show exc)
    handleCPUTemp setTemp = \case
      Right cpuTemp -> liftIO . Gtk.uiSingleRun . setTemp $ tempVal cpuTemp
      Left exc -> logS (T.pack "CPU") LevelWarn $ logStrf "CPU error (temp) : $1" (show exc)

data MainboardView = MainboardView
  { mainboardWidget :: !Gtk.Widget
  , mainClicks :: Source ()
  , setMemFill :: Sink Double
  , setCPUFill :: Sink Double
  , setCPUTemp :: Sink Temperature
  }

mainboardView :: T.Text -> IO MainboardView
mainboardView uiFile = do
  glibType @Gtk.ImageBar -- Ensures ImageBar is registered
  Gtk.buildFromFile uiFile $ do
    Just mainboardWidget <- Gtk.getElement (T.pack "mainboard") Gtk.Widget
    Just memBar <- Gtk.getElement (T.pack "mainboard-mem") Gtk.ImageBar
    Just cpuBar <- Gtk.getElement (T.pack "mainboard-cpu") Gtk.ImageBar

    let setMemFill = Gtk.imageBarSetFill memBar
        setCPUFill = Gtk.imageBarSetFill cpuBar
        setCPUTemp tempV = #getStyleContext cpuBar >>= Gtk.updateCssClass tempClass [tempV]

    (mainClicks, onClick) <- liftIO sourceSink
    Gtk.addCallback (T.pack "mainboard-open") $ onClick ()
    pure MainboardView{..}
