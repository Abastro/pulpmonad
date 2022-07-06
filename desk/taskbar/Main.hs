{-# LANGUAGE OverloadedLabels #-}

module Main where

import Control.Monad
import Control.Monad.IO.Unlift
import Data.Foldable
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
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
import Gtk.Styles qualified as Gtk
import Gtk.Window qualified as Gtk
import Status.X11.WMStatus (DesktopStat (..))
import System.Applet.Clocks qualified as App
import System.Applet.DesktopVisual qualified as App
import System.Applet.Layout qualified as App
import System.Applet.SysCtrl qualified as App
import System.Applet.SysTray qualified as App
import System.Applet.SystemDisplay qualified as App
import System.Applet.WMCtrl qualified as App
import System.Environment
import System.Exit
import System.Log.LogPrint (LogLevel (..), defLogFormat)
import System.Posix.Signals (sigINT)
import System.Pulp.PulpEnv
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

data BarWinArgs = BarWinArgs
  { barDockPos :: !Gtk.DockPos
  , barDockSize :: !Gtk.DockSize
  , barDockSpan :: !Gtk.DockSpan
  , barTitle :: !T.Text
  }

-- | Creates and links the taskbar window. Does not show the window.
taskbarWindow :: Gtk.Application -> BarWinArgs -> Gtk.Widget -> PulpIO Gtk.Window
taskbarWindow app BarWinArgs{..} content = do
  window <-
    new
      Gtk.Window
      [ #type := Gtk.WindowTypeToplevel
      , #title := barTitle
      ]
  #setKeepBelow window True
  Gtk.windowSetDock window $ Gtk.DockArg barDockPos barDockSize barDockSpan Nothing
  Gtk.windowSetTransparent window

  #add window content
  #getStyleContext content >>= flip #addClass (T.pack "pulp-bar")

  #addWindow app window
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

    dataDirToUse = if isTest then "XMONAD_CONFIG_DIR" else "XMONAD_DATA_DIR"

    cssProvMain :: IO Gtk.CssProvider
    cssProvMain = do
      css <- new Gtk.CssProvider []
      dataDir <- liftIO $ getEnv dataDirToUse
      #loadFromPath css $ T.pack (dataDir </> "styles" </> "pulp-taskbar.css")
      pure css

    iconThemeSetup :: IO ()
    iconThemeSetup = do
      dataDir <- getEnv dataDirToUse
      defaultTheme <- Gtk.iconThemeGetDefault
      #appendSearchPath defaultTheme (dataDir </> "asset" </> "icons")

    activating :: Gtk.Application -> PulpIO ()
    activating app = withRunInIO $ \unlift -> do
      cssProvMain >>= flip Gtk.defScreenAddStyleContext Gtk.STYLE_PROVIDER_PRIORITY_USER
      iconThemeSetup

      left <- unlift $ taskbarWindow app leftArgs =<< leftBox
      center <- unlift $ taskbarWindow app centerArgs =<< centerBox
      right <- unlift $ taskbarWindow app rightArgs =<< rightBox
      traverse_ #showAll [left, center, right]

    leftArgs =
      BarWinArgs
        { barDockPos = dockPos
        , barDockSize = Gtk.AbsoluteSize 32
        , barDockSpan = Gtk.DockSpan 0 (1 / 6)
        , barTitle = T.pack "Pulp Statusbar"
        }
    leftBox :: PulpIO Gtk.Widget
    leftBox = do
      box <- Gtk.boxNew Gtk.OrientationHorizontal 2
      #setName box (T.pack "pulp-statusbar")
      traverse_ (addToBegin box)
        =<< sequenceA [App.sysCtrlBtn, App.wmCtrlBtn]
      traverse_ (addToEnd box)
        =<< sequenceA [App.textClock "%b %_d (%a)\n%H:%M %p"]
      Gtk.toWidget box

    centerArgs =
      BarWinArgs
        { barDockPos = dockPos
        , barDockSize = Gtk.AbsoluteSize 36
        , barDockSpan = Gtk.DockSpan (1 / 6) (5 / 6)
        , barTitle = T.pack "Pulp Taskbar"
        }
    centerBox :: PulpIO Gtk.Widget
    centerBox = do
      box <- Gtk.boxNew Gtk.OrientationHorizontal 2
      #setName box (T.pack "pulp-taskbar")
      #setCenterWidget box . Just =<< App.deskVisualizer deskVisDeskSetup deskVisWinSetup
      traverse_ (addToBegin box)
        =<< sequenceA
          [ App.layout App.LayoutArg{layoutPrettyName = (T.pack "\xf0ce " <>)}
          ]
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
        , barDockSize = Gtk.AbsoluteSize 32
        , barDockSpan = Gtk.DockSpan (5 / 6) 1
        , barTitle = T.pack "Pulp Systemtray"
        }
    rightBox :: PulpIO Gtk.Widget
    rightBox = do
      box <- Gtk.boxNew Gtk.OrientationHorizontal 2
      #setName box (T.pack "pulp-systemtray")
      addToEnd box =<< liftIO (App.systemTray sysTrayArgs)
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
