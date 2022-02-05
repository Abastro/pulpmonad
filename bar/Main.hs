{-# LANGUAGE OverloadedStrings #-}
import System.Taffybar
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig hiding ( barPadding )
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingBar

cpuCfg :: BarConfig
cpuCfg = (defaultBarConfig $ const (0.4, 0.2, 0))
  { barWidth = 25, barPadding = 4 }
cpuCallback :: IO Double
cpuCallback = do
  (_, _, totalLoad) <- cpuLoad
  return totalLoad

memCfg :: BarConfig
memCfg = (defaultBarConfig $ const (0.1, 0.6, 0.9))
  { barWidth = 25, barPadding = 4 }

memCallback :: IO Double
memCallback = do
  mi <- parseMeminfo
  pure (memoryUsedRatio mi)

main :: IO ()
main = do
  startTaffybar $ toTaffyConfig simpleConfig
  where
    clock = textClockNewWith defaultClockConfig
      { clockFormatString = "%a %b %_d %H:%M %p" }
    cpu = pollingBarNew cpuCfg 0.1 cpuCallback
    mem = pollingBarNew memCfg 0.5 memCallback
    workspaces = workspacesNew defaultWorkspacesConfig
      { showWorkspaceFn = hideEmpty }
    simpleConfig = defaultSimpleTaffyConfig
      { startWidgets = [ workspaces ]
      , centerWidgets = [ clock ]
      , endWidgets = [ sniTrayNew, mem, cpu, batteryIconNew ]
      , barPosition = Top
      , barHeight = 40
      }
