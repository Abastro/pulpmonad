{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BroadcastChan
import Control.Monad.Reader
import Data.Map qualified as M
import Data.Maybe
import Data.Ratio ((%))
import Data.Text qualified as T
import Defines
import GtkCommons qualified as Gtk
import System.Environment
import System.Taffybar (startTaffybar)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.Battery (BatteryInfo (..), getDisplayBatteryChan, getDisplayBatteryInfo)
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig hiding
  ( barPadding,
  )
import System.Taffybar.Widget
import XMonad.ManageHook
import XMonad.StackSet (RationalRect (..))
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)
import XMonad.Util.Run

setupIcons :: FilePath -> TaffyIO ()
setupIcons mainDir = do
  defaultTheme <- Gtk.iconThemeGetDefault
  Gtk.iconThemeAppendSearchPath defaultTheme (mainDir </> "asset" </> "icons")

batWidget :: TaffyIO Gtk.Widget
batWidget = do
  -- The display
  disp <- do
    chan <- getDisplayBatteryChan
    getBatName <- asks (runReaderT $ T.pack . batteryIconName <$> getDisplayBatteryInfo)
    Gtk.iconNewChanneling (readBChan <$> newBChanListener chan) getBatName

  ev <- Gtk.buttonNewWith disp $ safeSpawn "gnome-control-center" ["power"]
  ev <$ Gtk.widgetShowAll ev

cpuCallback :: IO Double
cpuCallback = do
  (_, _, totalLoad) <- cpuLoad
  return totalLoad

memCallback :: IO Double
memCallback = memoryUsedRatio <$> parseMeminfo

mainboardWidget :: TaffyIO Gtk.Widget
mainboardWidget = do
  mem <- do
    hack <- Gtk.iconNewFromName "ram-000"
    fg <- Gtk.iconNewFromName "ram-000"
    let barRect = RationalRect (13 % 32) (8 % 32) (19 % 32) (24 % 32)
    bar <- Gtk.barNewPolling barRect (0.1, 0.6, 0.9) 0.5 memCallback
    Gtk.overlayed hack [bar, fg]
  Gtk.setWidgetHalign mem Gtk.AlignStart

  cpu <- do
    -- MAYBE Use temperature
    icon <- Gtk.iconNewPolling 0.1 $ do
      cpu :: Int <- round . (* 5) <$> cpuCallback
      pure (cpuN cpu)

    let barRect = RationalRect (28 % 64) (25 % 64) (36 % 64) (39 % 64)
    bar <- Gtk.barNewPolling barRect (0.9, 0.6, 0.1) 0.1 cpuCallback
    Gtk.overlayed icon [bar]
  Gtk.setWidgetHalign cpu Gtk.AlignEnd

  bg <- do
    img <- Gtk.imageNew -- Will set image later
    Gtk.widgetSetSizeRequest img 56 32
    Gtk.toWidget img

  disp <- Gtk.overlayed bg [mem, cpu]
  ev <- Gtk.buttonNewWith disp $ safeSpawn "gnome-system-monitor" ["-r"]
  ev <$ Gtk.widgetShowAll ev
  where
    cpuN n = T.pack $ printf "cpu-%03d" (n * 20)

workspaceMaps :: M.Map String String
workspaceMaps =
  M.fromList
    [ (wmain, "\xe3af"),
      (docs, "\xf0c7"),
      (code, "\xf121"),
      (term, "\xf120"),
      (chat, "\xf4ad"),
      (pics, "\xf03e"),
      (game, "\xf43c")
    ]

main :: IO ()
main = do
  mainDir <- getEnv "XMONAD_CONFIG_DIR"
  startTaffybar $
    toTaffyConfig
      defaultSimpleTaffyConfig
        { startupHook = setupIcons mainDir,
          startWidgets = [workspaces],
          centerWidgets = [clock],
          endWidgets = [mainboardWidget, batWidget, sniTrayNew],
          barPosition = Top,
          barHeight = read "ExactSize 40",
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
