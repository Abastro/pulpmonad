{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Task
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Map qualified as M
import Data.Maybe
import Data.Ord (clamp)
import Data.Ratio ((%))
import Data.Text qualified as T
import Data.Traversable
import Defines
import GI.Gio.Interfaces.AppInfo
import GI.Gio.Objects.DesktopAppInfo
import GI.Gtk.Objects.IconInfo qualified as UI
import GI.Gtk.Objects.IconTheme qualified as UI
import Status.HWStatus
import System.Environment
import System.Environment.XDG.DesktopEntry
import System.Log.Logger
import System.Taffybar (startTaffybar)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.SimpleConfig
import System.Taffybar.Util ((<|||>))
import System.Taffybar.Widget
import System.Taffybar.WindowIcon
import UI.Commons qualified as UI
import UI.Containers qualified as UI
import UI.Singles qualified as UI
import UI.X11.Desktops qualified as UI
import XMonad.ManageHook
import XMonad.StackSet (RationalRect (..))
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)
import XMonad.Util.Run
import System.Log.Handler.Simple
import System.IO

-- | Taffybar does it wrong way in so many degrees, DUH
-- Band-aid for now.
windowIconFromDEntry :: WindowIconPixbufGetter
windowIconFromDEntry size winData = getWindowIconForAllClasses throughDEntry size (windowClass winData)
  where
    -- Even gi-gio betrayed me... At least, this workaround does not look overly horrible.
    throughDEntry size cl = runMaybeT $ do
      entryName <- deFilename <$> MaybeT (getDirectoryEntryByClass cl)
      deskAppInfo <- MaybeT $ desktopAppInfoNewFromFilename entryName
      gIcon <- MaybeT (appInfoGetIcon deskAppInfo)

      defaultTheme <- UI.iconThemeGetDefault
      iconInfo <- MaybeT $ UI.iconThemeLookupByGicon defaultTheme gIcon size []
      UI.iconInfoLoadIcon iconInfo

setupIcons :: FilePath -> TaffyIO ()
setupIcons mainDir = do
  defaultTheme <- UI.iconThemeGetDefault
  UI.iconThemeAppendSearchPath defaultTheme (mainDir </> "asset" </> "icons")

batWidget :: TaffyIO UI.Widget
batWidget = do
  -- The display
  let batName level = \case
        Charging -> T.pack $ printf "battery-level-%d-charging-symbolic" level
        _ -> T.pack $ printf "battery-level-%d-symbolic" level
  widBat <-
    startRegular 500 batStat >>= traverse \task ->
      UI.iconNewTask UI.IconSizeDnd task \BatStat{capacity, batStatus} ->
        batName ((capacity `div` 10) * 10) batStatus

  ev <- UI.buttonNewWith widBat $ safeSpawn "gnome-control-center" ["power"]
  ev <$ UI.widgetShowAll ev

mainboardWidget :: TaffyIO UI.Widget
mainboardWidget = do
  widMem <-
    startRegular 500 memStat >>= traverse \task -> do
      hack <- UI.iconNewFromName UI.IconSizeDnd "ram-000"
      fg <- UI.iconNewFromName UI.IconSizeDnd "ram-000"
      let barRect = RationalRect (13 % 32) (8 % 32) (19 % 32) (24 % 32)
      bar <- UI.barNewTask barRect (0.1, 0.6, 0.9) task (memUsed . memRatios)
      UI.overlayed hack [bar, fg]
  traverse_ (`UI.setWidgetHalign` UI.AlignStart) widMem

  widCPU <- do
    cpuUse <- startRegular 50 (cpuDelta 50)
    cpuTemp <- startRegular 100 cpuTemp
    for ((,) <$> cpuUse <*> cpuTemp) \(taskUse, taskTemp) -> do
      let cpuN n = T.pack $ printf "cpu-%03d" (n * 20)
      fg <- UI.iconNewTask UI.IconSizeDnd taskTemp \temp ->
        -- Will operate on range 20C - 120C
        let tmpInd :: Int = round $ clamp (0, 100) (temp - 20) * 0.05 in cpuN tmpInd
      let barRect = RationalRect (28 % 64) (25 % 64) (36 % 64) (39 % 64)
      bar <- UI.barNewTask barRect (0.9, 0.6, 0.1) taskUse (cpuUsed . cpuRatios)
      UI.overlayed fg [bar]
  traverse_ (`UI.setWidgetHalign` UI.AlignEnd) widCPU

  bg <- do
    img <- UI.imageNew -- Will set image later
    UI.widgetSetSizeRequest img 56 32
    UI.toWidget img

  disp <- UI.overlayed bg (toList widMem <> toList widCPU)
  ev <- UI.buttonNewWith (Just disp) $ safeSpawn "gnome-system-monitor" ["-r"]
  ev <$ UI.widgetShowAll ev

workspaceMaps :: M.Map String String
workspaceMaps =
  M.fromList
    [ (wmain, "\xe3af")
    , (docs, "\xf0c7")
    , (code, "\xf121")
    , (term, "\xf120")
    , (chat, "\xf4ad")
    , (pics, "\xf03e")
    , (game, "\xf43c")
    ]

main :: IO ()
main = do
  mainDir <- getEnv "XMONAD_CONFIG_DIR"
  startTaffybar $
    toTaffyConfig
      defaultSimpleTaffyConfig
        { startupHook = setupIcons mainDir
        , startWidgets = [workspaces]
        , centerWidgets = [clock, _desktopVis]
        , endWidgets = [mainboardWidget, batWidget, sniTrayNew]
        , barPosition = Top
        , barHeight = read "ExactSize 40"
        , cssPaths = [mainDir </> "styles" </> "taffybar.css"]
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
          { showWorkspaceFn = hideEmpty <&&> ((/= scratchpadWorkspaceTag) . workspaceName)
          , labelSetter = pure . getName . workspaceName
          , getWindowIconPixbuf = windowIconFromDEntry <|||> getWindowIconPixbuf defaultWorkspacesConfig
          }

    mayLabel n = T.pack <$> workspaceMaps M.!? T.unpack n
    _desktopVis :: TaffyIO UI.Widget
    _desktopVis = do
      liftIO $ do
        -- Wat in tarnation, having to do just for logging?
        logger <- getLogger "DeskVis"
        handler <- streamHandler stderr INFO
        saveGlobalLogger $ setLevel INFO . setHandlers [handler] $ logger
      liftIO $ infoM "DeskVis" "Starting desktop visualizer..."
      UI.deskVisNew (fromMaybe "NONE" . (>>= mayLabel)) UI.defImageSetter
