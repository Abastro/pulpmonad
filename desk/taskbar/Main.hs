module Main where

import Control.Monad
import Control.Monad.IO.Unlift
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Defines
import GI.GLib.Constants qualified as Glib
import GI.GLib.Functions qualified as Glib
import GI.Gio.Flags qualified as Gio
import GI.Gtk.Objects.Box qualified as Gtk
import GI.Gtk.Objects.IconTheme qualified as Gtk
import Gtk.Application qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.Styles qualified as Gtk
import Gtk.Window qualified as Gtk
import Status.X11.WMStatus (DesktopStat (..))
import System.Applet.Clocks qualified as App
import System.Applet.DesktopVisual qualified as App
import System.Applet.SysTray qualified as App
import System.Applet.SystemDisplay qualified as App
import System.Environment
import System.Exit
import System.Log.LogPrint (LogLevel (..), defLogFormat)
import System.Posix.Signals (sigINT)
import System.Pulp.PulpEnv
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
  { barDockPos :: !Gtk.DockPos
  , barDockSize :: !Gtk.DockSize
  , barDockSpan :: !Gtk.DockSpan
  , barTitle :: !T.Text
  }

-- | Creates and links the taskbar window. Does not show the window.
taskbarWindow :: Gtk.Application -> BarWinArgs -> Gtk.Widget -> PulpIO Gtk.Window
taskbarWindow app BarWinArgs{..} content = do
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle window barTitle
  Gtk.windowSetDock window $ Gtk.DockArg barDockPos barDockSize barDockSpan Nothing
  Gtk.windowSetKeepBelow window True
  Gtk.windowSetTransparent window

  Gtk.containerAdd window content

  Gtk.applicationAddWindow app window
  pure window

main :: IO ()
main = do
  isTest <- ("test" `elem`) <$> getArgs
  runWithArg isTest

runWithArg :: Bool -> IO ()
runWithArg isTest = runPulpIO PulpArg{loggerFormat = defLogFormat, loggerVerbosity = verbosity} $
  withRunInIO $ \unlift -> do
    Just app <- Gtk.applicationNew (Just $ T.pack "pulp.ui.taskbar") [Gio.ApplicationFlagsNonUnique]
    Gtk.onApplicationActivate app (unlift $ activating app)
    Glib.unixSignalAdd Glib.PRIORITY_DEFAULT (fromIntegral sigINT) $ True <$ Gtk.applicationQuit app
    status <- Gtk.applicationRun app Nothing
    when (status /= 0) . liftIO $ exitWith (ExitFailure $ fromIntegral status)
  where
    verbosity = if isTest then LevelDebug else LevelInfo
    dockPos = if isTest then Gtk.DockBottom else Gtk.DockTop

    cssProv :: IO Gtk.CssProvider
    cssProv = do
      css <- Gtk.cssProviderNew
      cfgDir <- liftIO $ getEnv "XMONAD_CONFIG_DIR"
      Gtk.cssProviderLoadFromPath css $ T.pack (cfgDir </> "styles" </> "pulp-taskbar.css")
      pure css

    iconThemeSetup :: IO ()
    iconThemeSetup = do
      mainDir <- getEnv "XMONAD_CONFIG_DIR"
      defaultTheme <- Gtk.iconThemeGetDefault
      Gtk.iconThemeAppendSearchPath defaultTheme (mainDir </> "asset" </> "icons")

    activating :: Gtk.Application -> PulpIO ()
    activating app = withRunInIO $ \unlift -> do
      cssProv >>= flip Gtk.defScreenAddStyleContext Gtk.STYLE_PROVIDER_PRIORITY_USER
      iconThemeSetup

      left <- unlift $ taskbarWindow app leftArgs =<< leftBox
      center <- unlift $ taskbarWindow app centerArgs =<< centerBox
      right <- unlift $ taskbarWindow app rightArgs =<< rightBox
      traverse_ Gtk.widgetShowAll [left, center, right]

    leftArgs =
      BarWinArgs
        { barDockPos = dockPos
        , barDockSize = Gtk.AbsoluteSize 36
        , barDockSpan = Gtk.DockSpan 0 (1 / 6)
        , barTitle = T.pack "Pulp Statusbar"
        }
    leftBox :: PulpIO Gtk.Widget
    leftBox = do
      box <- Gtk.boxNew Gtk.OrientationHorizontal 0
      Gtk.widgetSetName box (T.pack "pulp-statusbar")
      powerIcon <- View.imageStaticNew Gtk.IconSizeLargeToolbar True $ View.ImgSName (T.pack "system-shutdown-symbolic")
      traverse_ (addToBegin box)
        =<< sequenceA
          [Gtk.buttonNewWith (Just powerIcon) runPulpCtl]
      Gtk.toWidget box
      where
        runPulpCtl = do
          cacheDir <- getEnv "XMONAD_CACHE_DIR"
          safeSpawn (cacheDir </> "pulp-sysctl") []

    centerArgs =
      BarWinArgs
        { barDockPos = dockPos
        , barDockSize = Gtk.AbsoluteSize 40
        , barDockSpan = Gtk.DockSpan (1 / 6) (5 / 6)
        , barTitle = T.pack "Pulp Taskbar"
        }
    centerBox :: PulpIO Gtk.Widget
    centerBox = do
      box <- Gtk.boxNew Gtk.OrientationHorizontal 2
      Gtk.widgetSetName box (T.pack "pulp-taskbar")
      Gtk.boxSetCenterWidget box . Just =<< App.deskVisualizer deskVisDeskSetup deskVisWinSetup
      traverse_ (addToBegin box) =<< sequenceA [App.textClock "%b %_d (%a)\n%H:%M %p"]
      traverse_ (addToEnd box)
        =<< sequenceA
          [ App.mainboardDisplay Gtk.IconSizeLargeToolbar 42
          , App.batDisplay Gtk.IconSizeLargeToolbar
          ]
      Gtk.toWidget box
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
            { windowImgIcon = App.defImageIcon
            , windowIconSize = Gtk.IconSizeLargeToolbar
            }

    rightArgs =
      BarWinArgs
        { barDockPos = dockPos
        , barDockSize = Gtk.AbsoluteSize 36
        , barDockSpan = Gtk.DockSpan (5 / 6) 1
        , barTitle = T.pack "Pulp Systemtray"
        }
    rightBox :: PulpIO Gtk.Widget
    rightBox = do
      box <- Gtk.boxNew Gtk.OrientationHorizontal 2
      Gtk.widgetSetName box (T.pack "pulp-systemtray")
      liftIO $ addToEnd box =<< App.systemTray sysTrayArgs
      Gtk.toWidget box
      where
        sysTrayArgs =
          App.SysTrayArgs
            { App.trayOrientation = Gtk.OrientationHorizontal
            , App.trayIconSize = Gtk.IconSizeLargeToolbar
            , App.trayAlignBegin = False
            }

    addToBegin box wid = Gtk.boxPackStart box wid False False 0
    addToEnd box wid = Gtk.boxPackEnd box wid False False 0
