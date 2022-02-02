{-# LANGUAGE OverloadedStrings #-}
import System.Taffybar
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.PollingGraph

cpuCfg :: GraphConfig
cpuCfg = defaultGraphConfig
  { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5) ]
  , graphLabel = Just "cpu"
  }
cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [ totalLoad, systemLoad ]

memCfg :: GraphConfig
memCfg = defaultGraphConfig
  { graphDataColors = [ (0.129, 0.588, 0.953, 1) ]
  , graphLabel = Just "mem"
  }
memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

main :: IO ()
main = do
  startTaffybar $ toTaffyConfig simpleConfig
  where
    clock = textClockNewWith defaultClockConfig
    cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
    mem = pollingGraphNew memCfg 1.0 memCallback
    workspaces = workspacesNew defaultWorkspacesConfig
      { showWorkspaceFn = hideEmpty }
    simpleConfig = defaultSimpleTaffyConfig
      { startWidgets = []
      , centerWidgets = [ clock ]
      , endWidgets = [ sniTrayNew, mem, cpu, batteryIconNew ]
      , barPosition = Top
      , barHeight = 30
      }
