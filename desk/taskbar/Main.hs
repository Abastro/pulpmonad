module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Defines
import GI.GLib qualified as UI
import GI.Gtk.Objects.Box qualified as UI
import System.Environment
import System.Exit
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import System.Posix.Signals (sigINT)
import UI.Application qualified as UI
import UI.Commons qualified as UI
import UI.Containers qualified as UI
import UI.Styles qualified as UI
import UI.Window qualified as UI
import UI.X11.DesktopVisual qualified as UI

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
  Just app <- UI.applicationNew (Just $ T.pack "pulp.ui.taskbar") []
  UI.onApplicationActivate app (activating app)
  UI.unixSignalAdd UI.PRIORITY_DEFAULT (fromIntegral sigINT) $ True <$ UI.applicationQuit app
  status <- UI.applicationRun app Nothing
  when (status /= 0) $ exitWith (ExitFailure $ fromIntegral status)
  where
    mayLabel n = fromMaybe n $ T.pack <$> workspaceMaps M.!? T.unpack n

    deskVisDeskSetup =
      UI.DesktopSetup
        { UI.desktopLabeling = maybe (T.pack "X") mayLabel
        , UI.showDesktop = UI.defShowFn
        }
    deskVisWinSetup = UI.WindowSetup UI.defImageSetter

    desktopVis :: IO UI.Widget
    desktopVis = do
      liftIO $ do
        -- Wat in tarnation, having to do just for logging?
        -- Will get rid of it once I got time
        updateGlobalLogger rootLoggerName removeHandler
        handler <- streamHandler stdout INFO
        updateGlobalLogger "DeskVis" $ setLevel INFO . setHandlers [handler]
      liftIO $ infoM "DeskVis" "Starting desktop visualizer..."
      UI.deskVisualizer deskVisDeskSetup deskVisWinSetup

    cssProv :: IO UI.CssProvider
    cssProv = do
      css <- UI.cssProviderNew
      cfgDir <- getEnv "XMONAD_CONFIG_DIR"
      UI.cssProviderLoadFromPath css $ T.pack (cfgDir </> "styles" </> "pulp-taskbar.css")
      pure css

    activating :: UI.Application -> IO ()
    activating app = do
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
