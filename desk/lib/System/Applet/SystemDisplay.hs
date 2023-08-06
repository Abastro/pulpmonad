{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecursiveDo #-}

module System.Applet.SystemDisplay (
  batDisplay,
  mainboardDisplay,
) where

import Control.Event.Entry
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Bifunctor
import Data.Foldable
import Data.GI.Base.Attributes
import Data.GI.Base.BasicTypes
import Data.Int
import Data.Text qualified as T
import Data.Validation
import GI.Gtk.Objects.Image qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.ImageBar qualified as Gtk
import Gtk.Styles qualified as Gtk
import Gtk.Task qualified as Gtk
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Status.HWStatus
import System.FilePath
import System.Log.LogPrint
import System.Pulp.PulpPath
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

batDisplay :: (MonadUnliftIO m, MonadLog m) => Gtk.IconSize -> m Gtk.Widget
batDisplay _iconSize = withRunInIO $ \unlift -> do
  uiFile <- dataPath ("ui" </> "battery.ui")
  BatView{..} <- batView (T.pack uiFile)

  network <- compile $ do
    ticker <- liftIO (periodicSource 500) >>= sourceEvent
    clicks <- sourceEvent batClicks

    -- See Volume.hs for reasons
    batterySample <- mapEventIO (const getBattery) ticker
    initialBattery <- liftIO getBattery
    rec
      let batteryUpdate = filterApply ((/=) <$> battery) batterySample
      battery <- stepper initialBattery batteryUpdate

    syncBehavior battery (Gtk.uiSingleRun . setBatIcon . iconOf)
    syncBehavior battery (Gtk.uiSingleRun . setBatTooltip . textOf)
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
    textOf = \case
      Right BatStat{capacity, batStatus = Charging} ->
        T.pack $ printf "battery: %d%%, charging" capacity
      Right BatStat{capacity, batStatus = _} ->
        T.pack $ printf "battery: %d%%" capacity
      Left _ -> T.pack "battery not available"

    handleExc = \case
      Left exc -> logS (T.pack "Battery") LevelWarn $ logStrf "Battery error : $1" (show exc)
      Right _ -> pure ()

data BatView = BatView
  { batWidget :: !Gtk.Widget
  , batClicks :: Source ()
  , setBatIcon :: Sink T.Text
  , setBatTooltip :: Sink T.Text
  }

batView :: T.Text -> IO BatView
batView uiFile = Gtk.buildFromFile uiFile $ do
  Just batWidget <- Gtk.getElement (T.pack "battery") Gtk.Widget
  Just batIcon <- Gtk.getElement (T.pack "battery-icon") Gtk.Image

  let setBatIcon icon = set batIcon [#iconName := icon]
      setBatTooltip text = set batIcon [#tooltipText := text]
  (batClicks, onClick) <- liftIO sourceSink
  Gtk.addCallback (T.pack "battery-open") $ onClick ()
  pure BatView{..}

{-------------------------------------------------------------------
                            Mainboard
--------------------------------------------------------------------}

-- MAYBE Roll down "menu" showing status & settings

-- TODO Warning colors when too full

mainboardDisplay :: (MonadUnliftIO m, MonadLog m) => Gtk.IconSize -> Int32 -> m Gtk.Widget
mainboardDisplay _iconSize _mainWidth = withRunInIO $ \unlift -> do
  uiFile <- dataPath ("ui" </> "mainboard.ui")
  MainboardView{..} <- mainboardView (T.pack uiFile)

  network <- compile $ do
    clicks <- sourceEvent mainClicks
    memTicker <- liftIO (periodicSource 500) >>= sourceEvent
    cpuTicker <- liftIO (periodicSource 100) >>= sourceEvent
    memory <- pollingBehavior getMemory memTicker
    cpuTemp <- pollingBehavior getCPUTemp cpuTicker
    (evtCPUStat, cpuStat) <- pollingBehaviorWithEvent getCPUStat cpuTicker
    -- Usage is determined as time spent (difference of spot time)
    cpuUsage <- stepper (Right cpuZero) $ liftA2 diff <$> cpuStat <@> evtCPUStat
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
    getMemory = try @IOException $ memStat
    getCPUTemp = try @IOException $ cpuTemp
    getCPUStat = try @IOException $ fst <$> cpuStat

    diff old new = liftA2 (-) new old
    combineCPU use temp = validToEither $ (,) <$> useE <*> tempE
      where
        useE = case use of
          Left exc -> Failure ["<usage> " <> show exc]
          Right u -> Success u
        tempE = case temp of
          Left exc -> Failure ["<temperature> " <> show exc]
          Right t -> Success t

    memFillOf = either (const 0) (memUsed . memRatios)
    cpuInfoOf = \case
      Right (use, temp) -> (cpuUsed (cpuRatios use), tempVal temp)
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
  glibType @Gtk.ImageBar -- Ensures ImageBar is registered
  Gtk.buildFromFile uiFile $ do
    Just mainboardWidget <- Gtk.getElement (T.pack "mainboard") Gtk.Widget
    Just memBar <- Gtk.getElement (T.pack "mainboard-mem") Gtk.ImageBar
    Just cpuBar <- Gtk.getElement (T.pack "mainboard-cpu") Gtk.ImageBar

    let setMemIcon icon = Gtk.imageBarSetIcon memBar icon Gtk.IconSizeLargeToolbar
        setCPUIcon icon = Gtk.imageBarSetIcon cpuBar icon Gtk.IconSizeLargeToolbar
        setMemFill = Gtk.imageBarSetFill memBar
        setCPUFill = Gtk.imageBarSetFill cpuBar
        setCPUTemp tempV = #getStyleContext cpuBar >>= Gtk.updateCssClass tempClass [tempV]

    (mainClicks, onClick) <- liftIO sourceSink
    Gtk.addCallback (T.pack "mainboard-open") $ onClick ()
    pure MainboardView{..}
