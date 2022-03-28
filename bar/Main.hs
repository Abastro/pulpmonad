{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Map qualified as M
import Data.Maybe
import GI.Gdk
import GI.Gtk hiding (main)
import System.Directory
import System.FilePath
import System.Taffybar
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig hiding
  ( barPadding,
  )
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Icon
import System.Taffybar.Widget.Generic.PollingBar
import Text.Printf
import XMonad.Core

spawnOnClick :: String -> EventButton -> IO Bool
spawnOnClick str btn = do
  b <- getEventButtonButton btn
  True <$ when (b == 1) (spawn str)

batWidget :: TaffyIO Widget
batWidget = do
  -- The display
  disp <- batteryIconNew

  -- Add button events
  ev <- eventBoxNew
  containerAdd ev disp
  onWidgetButtonReleaseEvent ev $ spawnOnClick "gnome-control-center power"

  widgetShowAll ev
  toWidget ev

cpuCallback :: IO Double
cpuCallback = do
  (_, _, totalLoad) <- cpuLoad
  return totalLoad

cpuWidget :: FilePath -> TaffyIO Widget
cpuWidget home = do
  -- The display
  disp <- pollingIconImageWidgetNew (cpuN 0) 0.1 $ do
    cpu <- cpuCallback
    pure (cpuN . round $ cpu * 5)

  -- Add button events
  ev <- eventBoxNew
  containerAdd ev disp
  onWidgetButtonReleaseEvent ev $ spawnOnClick "gnome-system-monitor -r"

  widgetShowAll ev
  toWidget ev
  where
    cpuN :: Int -> FilePath
    cpuN n = home </> printf ".xmonad/asset/icons/cpu%d.png" n

memCallback :: IO Double
memCallback = memoryUsedRatio <$> parseMeminfo

memWidget :: FilePath -> TaffyIO Widget
memWidget home = do
  -- Foreground and the Bar
  fg <- iconImageWidgetNew memN
  bar <- pollingBarNew memCfg 0.5 memCallback
  barCtxt <- widgetGetStyleContext bar
  styleContextAddClass barCtxt "mem-bar"

  -- Overlay bg image above memory bar
  wid <- overlayNew
  containerAdd wid bar
  overlayAddOverlay wid fg

  -- Add button events
  ev <- eventBoxNew
  containerAdd ev wid
  onWidgetButtonReleaseEvent ev $ spawnOnClick "gnome-system-monitor -r"

  widgetShowAll ev
  toWidget ev
  where
    memN = home </> ".xmonad/asset/icons/ram.png"
    memCfg =
      (defaultBarConfig $ const (0.1, 0.6, 0.9)) {barWidth = 9, barPadding = 0}

workspaceMaps :: M.Map String String
workspaceMaps =
  M.fromList
    [ ("main", "\xe3af"),
      ("docs", "\xf0c7"),
      ("code", "\xf121"),
      ("term", "\xf120"),
      ("chat", "\xf4ad"),
      ("pics", "\xf03e")
    ]

main :: IO ()
main = do
  home <- getHomeDirectory
  startTaffybar $
    toTaffyConfig
      defaultSimpleTaffyConfig
        { startWidgets = [workspaces],
          centerWidgets = [clock],
          endWidgets = [sniTrayNew, memWidget home, cpuWidget home, batWidget],
          barPosition = Top,
          barHeight = 45,
          cssPath = Just $ home </> ".xmonad/styles/taffybar.css"
        }
  where
    clock =
      textClockNewWith
        defaultClockConfig
          { clockFormatString = "%a %b %_d %H:%M %p"
          }
    getName n = fromMaybe n $ workspaceMaps M.!? n
    workspaces =
      workspacesNew
        defaultWorkspacesConfig
          { showWorkspaceFn = hideEmpty,
            labelSetter = pure . getName . workspaceName
          }
