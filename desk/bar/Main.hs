{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable
import Data.Map qualified as M
import Data.Maybe
import Data.Ord (clamp)
import Data.Ratio ((%))
import Data.Text qualified as T
import Data.Traversable
import Defines
import UI.GtkCommons qualified as Gtk
import Status.HWStatus
import System.Environment
import System.Taffybar (startTaffybar)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import Task
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
  let batName level = \case
        Charging -> T.pack $ printf "battery-level-%d-charging-symbolic" level
        _ -> T.pack $ printf "battery-level-%d-symbolic" level
  widBat <-
    startRegular 500 batStat >>= traverse \task ->
      Gtk.iconNewTask Gtk.IconSizeDnd task \BatStat {capacity, batStatus} ->
        batName ((capacity `div` 10) * 10) batStatus

  ev <- Gtk.buttonNewWith widBat $ safeSpawn "gnome-control-center" ["power"]
  ev <$ Gtk.widgetShowAll ev

mainboardWidget :: TaffyIO Gtk.Widget
mainboardWidget = do
  widMem <-
    startRegular 500 memStat >>= traverse \task -> do
      hack <- Gtk.iconNewFromName Gtk.IconSizeDnd "ram-000"
      fg <- Gtk.iconNewFromName Gtk.IconSizeDnd "ram-000"
      let barRect = RationalRect (13 % 32) (8 % 32) (19 % 32) (24 % 32)
      bar <- Gtk.barNewTask barRect (0.1, 0.6, 0.9) task (memUsed . memRatios)
      Gtk.overlayed hack [bar, fg]
  traverse_ (`Gtk.setWidgetHalign` Gtk.AlignStart) widMem

  widCPU <- do
    cpuUse <- startRegular 50 (cpuDelta 50)
    cpuTemp <- startRegular 100 cpuTemp
    for ((,) <$> cpuUse <*> cpuTemp) \(taskUse, taskTemp) -> do
      let cpuN n = T.pack $ printf "cpu-%03d" (n * 20)
      fg <- Gtk.iconNewTask Gtk.IconSizeDnd taskTemp \temp ->
        -- Will operate on range 20C - 120C
        let tmpInd :: Int = round $ clamp (0, 100) (temp - 20) * 0.05 in cpuN tmpInd
      let barRect = RationalRect (28 % 64) (25 % 64) (36 % 64) (39 % 64)
      bar <- Gtk.barNewTask barRect (0.9, 0.6, 0.1) taskUse (cpuUsed . cpuRatios)
      Gtk.overlayed fg [bar]
  traverse_ (`Gtk.setWidgetHalign` Gtk.AlignEnd) widCPU

  bg <- do
    img <- Gtk.imageNew -- Will set image later
    Gtk.widgetSetSizeRequest img 56 32
    Gtk.toWidget img

  disp <- Gtk.overlayed bg (toList widMem <> toList widCPU)
  ev <- Gtk.buttonNewWith (Just disp) $ safeSpawn "gnome-system-monitor" ["-r"]
  ev <$ Gtk.widgetShowAll ev

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
