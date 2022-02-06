{-# LANGUAGE OverloadedStrings #-}

import           GI.Gtk                  hiding ( main )
import           System.Directory
import           System.FilePath
import           System.Taffybar
import           System.Taffybar.Context        ( TaffyIO )
import           System.Taffybar.Information.CPU
import           System.Taffybar.Information.Memory
import           System.Taffybar.SimpleConfig
                                         hiding ( barPadding )
import           System.Taffybar.Widget
import           System.Taffybar.Widget.Generic.Icon
import           System.Taffybar.Widget.Generic.PollingBar
import           Text.Printf

cpuCallback :: IO Double
cpuCallback = do
  (_, _, totalLoad) <- cpuLoad
  return totalLoad

cpuImg :: FilePath -> TaffyIO Widget
cpuImg home = pollingIconImageWidgetNew (cpuN 0) 0.1 $ do
  cpu <- cpuCallback
  pure (cpuN . round $ cpu * 5)
 where
  cpuN :: Int -> FilePath
  cpuN n = home </> printf ".xmonad/asset/icons/cpu%d.png" n

memCfg :: BarConfig
memCfg =
  (defaultBarConfig $ const (0.1, 0.6, 0.9)) { barWidth = 8, barPadding = 1 }

memCallback :: IO Double
memCallback = memoryUsedRatio <$> parseMeminfo

main :: IO ()
main = do
  home <- getHomeDirectory
  let memimg = iconImageWidgetNew $ home </> ".xmonad/asset/icons/ram.png"
  startTaffybar $ toTaffyConfig defaultSimpleTaffyConfig
    { startWidgets  = [workspaces]
    , centerWidgets = [clock]
    , endWidgets    = [sniTrayNew, mem, memimg, cpuImg home, batteryIconNew]
    , barPosition   = Top
    , barHeight     = 40
    , cssPath       = Just $ home </> ".xmonad/styles/taffybar.css"
    }
 where
  clock = textClockNewWith defaultClockConfig
    { clockFormatString = "%a %b %_d %H:%M %p"
    }
  mem = pollingBarNew memCfg 0.5 memCallback
  workspaces =
    workspacesNew defaultWorkspacesConfig { showWorkspaceFn = hideEmpty }
