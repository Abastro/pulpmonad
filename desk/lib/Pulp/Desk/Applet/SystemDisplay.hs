{-# LANGUAGE OverloadedLabels #-}

module Pulp.Desk.Applet.SystemDisplay (
  batDisplay,
  mainboardDisplay,
) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Bifunctor
import Data.Foldable
import Data.GI.Base.Attributes qualified as GI
import Data.GI.Base.BasicTypes qualified as GI
import Data.Int
import Data.Text qualified as T
import Data.Validation
import GI.Gtk.Objects.Image qualified as Gtk
import Pulp.Desk.Env.PulpEnv
import Pulp.Desk.PulpPath
import Pulp.Desk.Reactive.Entry
import Pulp.Desk.System.HWStatus qualified as HW
import Pulp.Desk.UI.Commons qualified as Gtk
import Pulp.Desk.UI.Reactive qualified as Gtk
import Pulp.Desk.UI.Styles qualified as Gtk
import Pulp.Desk.UI.Widget.ImageBar (ImageBar (..))
import Pulp.Desk.UI.Widget.ImageBar qualified as ImageBar
import Pulp.Desk.Utils.LogPrint
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
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

batDisplay :: Gtk.IconSize -> PulpIO Gtk.Widget
batDisplay _iconSize = withRunInIO $ \unlift -> do
  uiFile <- dataPath ("ui" </> "battery.ui")
  BatView{..} <- batView (T.pack uiFile)

  network <- compile $ do
    ticker <- sourceEvent (periodicSource 500)
    clicks <- sourceEvent batClicks
    battery <- pollingBehavior getBattery ticker

    syncBehavior battery (Gtk.uiSingleRun . setBatIcon . iconOf)
    syncBehavior battery (unlift . handleExc)
    reactimate (safeSpawn "gnome-control-center" ["power"] <$ clicks)
  actuate network
  pure batWidget
  where
    getBattery = try @IOException HW.batStat
    iconOf = \case
      Right HW.BatStat{capacity, batStatus = HW.Charging} ->
        T.pack $ printf "battery-level-%d-charging-symbolic" (levelOf capacity)
      Right HW.BatStat{capacity, batStatus = _} ->
        T.pack $ printf "battery-level-%d-symbolic" (levelOf capacity)
      Left _ -> T.pack "battery-missing-symbolic"
    levelOf capacity = (capacity `div` 10) * 10

    handleExc = \case
      Left exc -> logS (T.pack "Battery") LevelWarn $ logStrf "Battery error : $1" (show exc)
      Right _ -> pure ()

data BatView = BatView
  { batWidget :: !Gtk.Widget
  , batClicks :: Source ()
  , setBatIcon :: Sink T.Text
  }

batView :: T.Text -> IO BatView
batView uiFile = Gtk.buildFromFile uiFile $ do
  Just batWidget <- Gtk.getElement (T.pack "battery") Gtk.Widget
  Just batIcon <- Gtk.getElement (T.pack "battery-icon") Gtk.Image

  let setBatIcon icon = GI.set batIcon [#iconName GI.:= icon]
  (batClicks, onClick) <- liftIO sourceSink
  Gtk.addCallback (T.pack "battery-open") $ onClick ()
  pure BatView{..}

{-------------------------------------------------------------------
                            Mainboard
--------------------------------------------------------------------}

-- MAYBE Roll down "menu" showing status & settings

-- TODO Warning colors when too full

mainboardDisplay :: Gtk.IconSize -> Int32 -> PulpIO Gtk.Widget
mainboardDisplay _iconSize _mainWidth = withRunInIO $ \unlift -> do
  uiFile <- dataPath ("ui" </> "mainboard.ui")
  MainboardView{..} <- mainboardView (T.pack uiFile)

  network <- compile $ do
    clicks <- sourceEvent mainClicks
    memTicker <- sourceEvent (periodicSource 500)
    cpuTicker <- sourceEvent (periodicSource 100)
    memory <- pollingBehavior getMemory memTicker
    cpuTemp <- pollingBehavior getCPUTemp cpuTicker
    (evtCPUStat, cpuStat) <- pollingDiscrete getCPUStat cpuTicker
    -- Usage is determined as time spent (difference of spot time)
    cpuUsage <- stepper (Right HW.cpuZero) $ liftA2 diff <$> cpuStat <@> evtCPUStat
    let cpu = combineCPU <$> cpuUsage <*> cpuTemp

    syncBehavior memory (Gtk.uiSingleRun . setMemFill . memFillOf)
    syncBehavior memory (Gtk.uiSingleRun . setMemIcon . memIconOf)
    syncBehavior memory (unlift . handleMemExc)

    -- For now this feels better than handle action
    syncBehavior cpu (Gtk.uiSingleRun . uncurry (<>) . bimap setCPUFill setCPUTemp . cpuInfoOf)
    syncBehavior cpu (Gtk.uiSingleRun . setCPUIcon . cpuIconOf)
    syncBehavior cpu (unlift . handleCPUExc)
    reactimate (safeSpawn "gnome-system-monitor" ["-r"] <$ clicks)
  actuate network
  pure mainboardWidget
  where
    getMemory = try @IOException $ HW.memStat
    getCPUTemp = try @IOException $ HW.cpuTemp
    getCPUStat = try @IOException $ fst <$> HW.cpuStat

    diff old new = liftA2 (-) new old
    combineCPU use temp = validToEither $ (,) <$> useE <*> tempE
      where
        useE = case use of
          Left exc -> Failure ["<usage> " <> show exc]
          Right u -> Success u
        tempE = case temp of
          Left exc -> Failure ["<temperature> " <> show exc]
          Right t -> Success t

    memFillOf = either (const 0) (HW.memUsed . HW.memRatios)
    cpuInfoOf = \case
      Right (use, temp) -> (HW.cpuUsed (HW.cpuRatios use), tempVal temp)
      Left _ -> (0, T20)

    memIconOf = \case
      Right _ -> T.pack "ram-symbolic"
      Left _ -> T.pack "ram-missing-symbolic"
    cpuIconOf = \case
      Right _ -> T.pack "cpu-symbolic"
      Left _ -> T.pack "cpu-missing-symbolic"

    handleMemExc = \case
      Left exc -> logS (T.pack "Memory") LevelWarn $ logStrf "Memory error : $1" (show exc)
      Right _ -> pure ()
    handleCPUExc = \case
      Left excs -> traverse_ (logS (T.pack "CPU") LevelWarn . logStrf "CPU error : $1") excs
      Right _ -> pure ()

data MainboardView = MainboardView
  { mainboardWidget :: !Gtk.Widget
  , mainClicks :: Source ()
  , setCPUIcon :: Sink T.Text
  , setMemIcon :: Sink T.Text
  , setMemFill :: Sink Double
  , setCPUFill :: Sink Double
  , setCPUTemp :: Sink Temperature
  }

mainboardView :: T.Text -> IO MainboardView
mainboardView uiFile = do
  GI.glibType @ImageBar -- Ensures ImageBar is registered
  Gtk.buildFromFile uiFile $ do
    Just mainboardWidget <- Gtk.getElement (T.pack "mainboard") Gtk.Widget
    Just memBar <- Gtk.getElement (T.pack "mainboard-mem") AsImageBar
    Just cpuBar <- Gtk.getElement (T.pack "mainboard-cpu") AsImageBar

    let setMemIcon = ImageBar.setIcon memBar
        setMemFill = ImageBar.setFill memBar
        setCPUIcon = ImageBar.setIcon cpuBar
        setCPUFill = ImageBar.setFill cpuBar
        setCPUTemp tempV = cpuBar.getStyleContext >>= Gtk.updateCssClass tempClass [tempV]        

    (mainClicks, onClick) <- liftIO sourceSink
    Gtk.addCallback (T.pack "mainboard-open") $ onClick ()
    pure MainboardView{..}
