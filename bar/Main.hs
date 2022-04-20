{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.Map qualified as M
import Data.Maybe
import Defines
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
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
import XMonad
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)
import XMonad.Util.Run
import System.Environment

runOnClick :: IO () -> Gdk.EventButton -> IO Bool
runOnClick act btn = do
  b <- Gdk.getEventButtonButton btn
  True <$ when (b == 1) act

-- Battery is quite cumbersome to implement with gtk-declarative
batWidget :: TaffyIO Gtk.Widget
batWidget = do
  -- The display
  disp <- batteryIconNew

  -- Add button events
  ev <- Gtk.eventBoxNew
  Gtk.containerAdd ev disp
  Gtk.onWidgetButtonReleaseEvent ev $ runOnClick $ safeSpawn "gnome-control-center" ["power"]

  Gtk.widgetShowAll ev
  Gtk.toWidget ev

cpuCallback :: IO Double
cpuCallback = do
  (_, _, totalLoad) <- cpuLoad
  return totalLoad

cpuWidget :: FilePath -> TaffyIO Gtk.Widget
cpuWidget mainDir = do
  -- The display
  disp <- pollingIconImageWidgetNew (cpuN (0 :: Int)) 0.1 $ do
    cpu :: Int <- round . (* 5) <$> cpuCallback
    pure (cpuN cpu)

  -- TODO Animation for CPU?
  -- Add button events
  ev <- Gtk.eventBoxNew
  Gtk.containerAdd ev disp
  Gtk.onWidgetButtonReleaseEvent ev $ runOnClick $ safeSpawn "gnome-system-monitor" ["-r"]

  Gtk.widgetShowAll ev
  Gtk.toWidget ev
  where
    cpuN n = mainDir </> "asset" </> "icons" </> printf "cpu%d.png" n

memCallback :: IO Double
memCallback = memoryUsedRatio <$> parseMeminfo

memWidget :: FilePath -> TaffyIO Gtk.Widget
memWidget mainDir = do
  -- Foreground and the Bar
  fg <- iconImageWidgetNew memN
  bar <- pollingBarNew memCfg 0.5 memCallback
  barCtxt <- Gtk.widgetGetStyleContext bar
  Gtk.styleContextAddClass barCtxt "mem-bar"

  -- Overlay bg image above memory bar
  wid <- Gtk.overlayNew
  Gtk.containerAdd wid bar
  Gtk.overlayAddOverlay wid fg

  -- Add button events
  ev <- Gtk.eventBoxNew
  Gtk.containerAdd ev wid
  Gtk.onWidgetButtonReleaseEvent ev $ runOnClick $ safeSpawn "gnome-system-monitor" ["-r"]

  Gtk.widgetShowAll ev
  Gtk.toWidget ev
  where
    memN = mainDir </> "asset" </> "icons" </> "ram.png"
    memCfg =
      (defaultBarConfig $ const (0.1, 0.6, 0.9)) {barWidth = 9, barPadding = 0}

workspaceMaps :: M.Map String String
workspaceMaps =
  M.fromList
    [ (wmain, "\xe3af"),
      (docs, "\xf0c7"),
      (code, "\xf121"),
      (term, "\xf120"),
      (chat, "\xf4ad"),
      (pics, "\xf03e")
    ]

main :: IO ()
main = do
  mainDir <- getEnv "XMONAD_CONFIG_DIR"
  startTaffybar $
    toTaffyConfig
      defaultSimpleTaffyConfig
        { startWidgets = [workspaces],
          centerWidgets = [clock],
          endWidgets = [memWidget mainDir, cpuWidget mainDir, batWidget, sniTrayNew],
          barPosition = Top,
          barHeight = read "ExactSize 45",
          cssPaths = [mainDir </> "styles" </> "taffybar.css"]
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
          { showWorkspaceFn = hideEmpty <&&> ((/= scratchpadWorkspaceTag) . workspaceName),
            labelSetter = pure . getName . workspaceName
          }
