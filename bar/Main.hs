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

cpuWidget :: FilePath -> TaffyIO Widget
cpuWidget home = pollingIconImageWidgetNew (cpuN 0) 0.1 $ do
  cpu <- cpuCallback
  pure (cpuN . round $ cpu * 5)
 where
  cpuN :: Int -> FilePath
  cpuN n = home </> printf ".xmonad/asset/icons/cpu%d.png" n

memCallback :: IO Double
memCallback = memoryUsedRatio <$> parseMeminfo

memWidget :: FilePath -> TaffyIO Widget
memWidget home = do
  bg  <- iconImageWidgetNew memN
  bar <- pollingBarNew memCfg 0.5 memCallback
  wid <- overlayNew

  containerAdd wid bar
  overlayAddOverlay wid bg
  widgetShowAll wid
  toWidget wid
 where
  memN = home </> ".xmonad/asset/icons/ram.png"
  memCfg =
    (defaultBarConfig $ const (0.1, 0.6, 0.9)) { barWidth = 21, barPadding = 7 }


main :: IO ()
main = do
  home <- getHomeDirectory
  startTaffybar $ toTaffyConfig defaultSimpleTaffyConfig
    { startWidgets  = [workspaces]
    , centerWidgets = [clock]
    , endWidgets = [sniTrayNew, memWidget home, cpuWidget home, batteryIconNew]
    , barPosition   = Top
    , barHeight     = 45
    , cssPath       = Just $ home </> ".xmonad/styles/taffybar.css"
    }
 where
  clock = textClockNewWith defaultClockConfig
    { clockFormatString = "%a %b %_d %H:%M %p"
    }
  workspaces =
    workspacesNew defaultWorkspacesConfig { showWorkspaceFn = hideEmpty }
