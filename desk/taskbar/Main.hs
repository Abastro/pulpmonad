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
import System.Pulp.Applet.SysTray qualified as App
import System.Pulp.Applet.SystemDisplay qualified as App
import System.Pulp.PulpEnv
import UI.Application qualified as UI
import UI.Commons qualified as UI
import UI.Containers qualified as UI
import UI.Styles qualified as UI
import UI.Window qualified as UI
import View.Imagery qualified as View
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)
import XMonad.Util.Run (safeSpawn)

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

data BarWinArgs = BarWinArgs
  { barDockPos :: !UI.DockPos
  , barDockSize :: !UI.DockSize
  , barDockSpan :: !UI.DockSpan
  , barTitle :: !T.Text
  }

-- | Creates and links the taskbar window. Does not show the window.
taskbarWindow :: UI.Application -> BarWinArgs -> UI.Widget -> PulpIO UI.Window
taskbarWindow app BarWinArgs{..} content = do
  window <- UI.windowNew UI.WindowTypeToplevel
  UI.windowSetTitle window barTitle
  UI.windowSetDock window barDockPos barDockSize barDockSpan
  UI.windowSetKeepBelow window True
  UI.windowSetSkipPagerHint window True
  UI.windowSetSkipTaskbarHint window True
  UI.windowSetTransparent window

  UI.containerAdd window content

  UI.applicationAddWindow app window
  pure window

main :: IO ()
main = runPulpIO $
  withRunInIO $ \unlift -> do
    Just app <- UI.applicationNew (Just $ T.pack "pulp.ui.taskbar") []
    UI.onApplicationActivate app (unlift $ activating app)
    UI.unixSignalAdd UI.PRIORITY_DEFAULT (fromIntegral sigINT) $ True <$ UI.applicationQuit app
    status <- UI.applicationRun app Nothing
    when (status /= 0) . liftIO $ exitWith (ExitFailure $ fromIntegral status)
  where
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

      left <- unlift $ taskbarWindow app leftArgs =<< leftBox
      center <- unlift $ taskbarWindow app centerArgs =<< centerBox
      right <- unlift $ taskbarWindow app rightArgs =<< rightBox
      traverse_ UI.widgetShowAll [left, center, right]

    leftArgs =
      BarWinArgs
        { barDockPos = UI.DockTop
        , barDockSize = UI.AbsoluteSize 36
        , barDockSpan = UI.DockSpan 0 (1 / 6)
        , barTitle = T.pack "Pulp Statusbar"
        }
    leftBox :: PulpIO UI.Widget
    leftBox = do
      box <- UI.boxNew UI.OrientationHorizontal 0
      UI.widgetSetName box  (T.pack "pulp-statusbar")
      UI.widgetGetStyleContext box >>= flip UI.styleContextAddClass (T.pack "statusbar-box")
      powerIcon <- View.imageStaticNew UI.IconSizeLargeToolbar $ View.ImgSName (T.pack "system-shutdown-symbolic")
      traverse_ (addToBegin box)
        =<< sequenceA
          [UI.buttonNewWith (Just powerIcon) runPulpCtl]
      traverse_ (addToEnd box)
        =<< sequenceA
          [ App.mainboardDisplay UI.IconSizeLargeToolbar 42
          , App.batDisplay UI.IconSizeLargeToolbar
          ]
      UI.toWidget box
      where
        runPulpCtl = do
          cacheDir <- getEnv "XMONAD_CACHE_DIR"
          safeSpawn (cacheDir </> "pulp-sysctl") []

    centerArgs =
      BarWinArgs
        { barDockPos = UI.DockTop
        , barDockSize = UI.AbsoluteSize 40
        , barDockSpan = UI.DockSpan (1 / 6) (5 / 6)
        , barTitle = T.pack "Pulp Taskbar"
        }
    centerBox :: PulpIO UI.Widget
    centerBox = do
      box <- UI.boxNew UI.OrientationHorizontal 2
      UI.widgetSetName box (T.pack "pulp-taskbar")
      UI.boxSetCenterWidget box . Just =<< App.deskVisualizer deskVisDeskSetup deskVisWinSetup
      traverse_ (addToBegin box) =<< sequenceA [App.textClock "%b %_d (%a)\n%H:%M %p"]
      UI.toWidget box
      where
        mayLabel n = maybe n T.pack $ workspaceMaps M.!? T.unpack n
        deskVisDeskSetup =
          App.DesktopSetup
            { App.desktopLabeling = maybe (T.pack "X") mayLabel
            , App.showDesktop = \stat@DesktopStat{desktopName} n ->
                App.defShowFn stat n && desktopName /= Just (T.pack scratchpadWorkspaceTag)
            }
        deskVisWinSetup =
          App.WindowSetup
            { windowImgSetter = App.defImageSetter
            , windowIconSize = UI.IconSizeLargeToolbar
            }

    rightArgs =
      BarWinArgs
        { barDockPos = UI.DockTop
        , barDockSize = UI.AbsoluteSize 36
        , barDockSpan = UI.DockSpan (5 / 6) 1
        , barTitle = T.pack "Pulp Systemtray"
        }
    rightBox :: PulpIO UI.Widget
    rightBox = do
      box <- UI.boxNew UI.OrientationHorizontal 2
      UI.widgetSetName box (T.pack "pulp-systemtray")
      liftIO $ addToEnd box =<< App.systemTray sysTrayArgs
      UI.toWidget box
      where
        sysTrayArgs =
          App.SysTrayArgs
            { App.trayOrientation = UI.OrientationHorizontal
            , App.trayIconSize = UI.IconSizeLargeToolbar
            , App.trayAlignBegin = False
            }

    addToBegin box wid = UI.boxPackStart box wid False False 0
    addToEnd box wid = UI.boxPackEnd box wid False False 0
