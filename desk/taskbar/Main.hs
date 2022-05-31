module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Defines
import GI.GLib qualified as UI
import GI.Gtk.Objects.Box qualified as UI
import Status.X11.WMStatus (DesktopStat (..))
import System.Environment
import System.Exit
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import System.Posix.Signals (sigINT)
import System.Pulp.Applet.DesktopVisual qualified as App
import System.Pulp.PulpEnv
import UI.Application qualified as UI
import UI.Commons qualified as UI
import UI.Containers qualified as UI
import UI.Styles qualified as UI
import UI.Window qualified as UI
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)

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

-- TODO Diagnose why heap grows to such a size
main :: IO ()
main = do
  Just app <- UI.applicationNew (Just $ T.pack "pulp.ui.taskbar") []
  UI.onApplicationActivate app (activating app)
  UI.unixSignalAdd UI.PRIORITY_DEFAULT (fromIntegral sigINT) $ True <$ UI.applicationQuit app
  status <- UI.applicationRun app Nothing
  when (status /= 0) $ exitWith (ExitFailure $ fromIntegral status)
  where
    mayLabel n = fromMaybe n $ T.pack <$> workspaceMaps M.!? T.unpack n

    deskVisDeskSetup =
      App.DesktopSetup
        { App.desktopLabeling = maybe (T.pack "X") mayLabel
        , App.showDesktop = \stat@DesktopStat{desktopName} n ->
            App.defShowFn stat n && desktopName /= Just (T.pack scratchpadWorkspaceTag)
        }
    deskVisWinSetup = App.WindowSetup App.defImageSetter

    desktopVis :: IO UI.Widget
    desktopVis = do
      liftIO $ do
        -- Wat in tarnation, having to do just for logging?
        -- Will get rid of it once I got time
        updateGlobalLogger rootLoggerName removeHandler
        handler <- streamHandler stdout INFO
        updateGlobalLogger "DeskVis" $ setLevel INFO . setHandlers [handler]
      liftIO $ infoM "DeskVis" "Starting desktop visualizer..."
      App.deskVisualizer deskVisDeskSetup deskVisWinSetup

    cssProv :: IO UI.CssProvider
    cssProv = do
      css <- UI.cssProviderNew
      cfgDir <- getEnv "XMONAD_CONFIG_DIR"
      UI.cssProviderLoadFromPath css $ T.pack (cfgDir </> "styles" </> "pulp-taskbar.css")
      pure css

    activating :: UI.Application -> IO ()
    activating app = do
      runPulpIO $ do
        undefined

      cssProv >>= flip UI.defScreenAddStyleContext UI.STYLE_PROVIDER_PRIORITY_USER

      window <- UI.appWindowNew app
      UI.windowSetTitle window (T.pack "Pulp Taskbar")
      UI.windowSetDock window UI.DockBottom (UI.AbsoluteSize 40) (UI.DockSpan (1 / 6) (5 / 6))
      UI.windowSetKeepAbove window True
      UI.windowSetSkipPagerHint window True
      UI.windowSetSkipTaskbarHint window True

      UI.windowSetTransparent window

      UI.containerAdd window =<< barBox

      UI.widgetShowAll window

    barBox = do
      box <- UI.boxNew UI.OrientationHorizontal 5
      UI.widgetGetStyleContext box >>= flip UI.styleContextAddClass (T.pack "taskbar-box")
      UI.boxSetCenterWidget box . Just =<< desktopVis
      UI.toWidget box
