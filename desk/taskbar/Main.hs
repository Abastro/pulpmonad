module Main where

import Control.Monad
import Control.Monad.IO.Unlift
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Defines
import GI.GLib qualified as UI
import GI.Gtk.Objects.Box qualified as UI
import GI.Gtk.Objects.IconTheme qualified as UI
import Status.X11.WMStatus (DesktopStat (..))
import System.Environment
import System.Exit
import System.Posix.Signals (sigINT)
import System.Pulp.Applet.Clocks qualified as App
import System.Pulp.Applet.DesktopVisual qualified as App
import System.Pulp.Applet.SystemDisplay qualified as App
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
main = runPulpIO $
  withRunInIO $ \unlift -> do
    Just app <- UI.applicationNew (Just $ T.pack "pulp.ui.taskbar") []
    UI.onApplicationActivate app (unlift $ activating app)
    UI.unixSignalAdd UI.PRIORITY_DEFAULT (fromIntegral sigINT) $ True <$ UI.applicationQuit app
    status <- UI.applicationRun app Nothing
    when (status /= 0) . liftIO $ exitWith (ExitFailure $ fromIntegral status)
  where
    mayLabel n = maybe n T.pack $ workspaceMaps M.!? T.unpack n

    deskVisDeskSetup =
      App.DesktopSetup
        { App.desktopLabeling = maybe (T.pack "X") mayLabel
        , App.showDesktop = \stat@DesktopStat{desktopName} n ->
            App.defShowFn stat n && desktopName /= Just (T.pack scratchpadWorkspaceTag)
        }
    deskVisWinSetup = App.WindowSetup App.defImageSetter

    desktopVis :: PulpIO UI.Widget
    desktopVis = do
      App.deskVisualizer deskVisDeskSetup deskVisWinSetup

    cssProv :: IO UI.CssProvider
    cssProv = do
      css <- UI.cssProviderNew
      cfgDir <- liftIO $ getEnv "XMONAD_CONFIG_DIR"
      UI.cssProviderLoadFromPath css $ T.pack (cfgDir </> "styles" </> "pulp-taskbar.css")
      pure css

    iconThemeSetup :: IO ()
    iconThemeSetup = do
      mainDir <- getEnv "XMONAD_CONFIG_DIR"
      defaultTheme <- UI.iconThemeGetDefault
      UI.iconThemeAppendSearchPath defaultTheme (mainDir </> "asset" </> "icons")

    activating :: UI.Application -> PulpIO ()
    activating app = withRunInIO $ \unlift -> do
      cssProv >>= flip UI.defScreenAddStyleContext UI.STYLE_PROVIDER_PRIORITY_USER
      iconThemeSetup

      window <- UI.appWindowNew app
      UI.windowSetTitle window (T.pack "Pulp Taskbar")
      UI.windowSetDock window UI.DockBottom (UI.AbsoluteSize 40) (UI.DockSpan (1 / 6) (5 / 6))
      UI.windowSetKeepAbove window True
      UI.windowSetSkipPagerHint window True
      UI.windowSetSkipTaskbarHint window True

      UI.windowSetTransparent window

      UI.containerAdd window =<< unlift barBox

      UI.widgetShowAll window

    barBox :: PulpIO UI.Widget
    barBox = do
      box <- UI.boxNew UI.OrientationHorizontal 5
      UI.widgetGetStyleContext box >>= flip UI.styleContextAddClass (T.pack "taskbar-box")
      UI.boxSetCenterWidget box . Just =<< desktopVis
      traverse_ (addToBegin box) =<< sequenceA [App.textClock "%b %_d (%a) %H:%M %p"]
      traverse_ (addToEnd box) =<< sequenceA [App.mainboardDisplay, App.batDisplay]
      UI.toWidget box
      where
        addToBegin box wid = UI.boxPackStart box wid False False 0
        addToEnd box wid = UI.boxPackEnd box wid False False 0
