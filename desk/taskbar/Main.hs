{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Control.Monad.IO.Unlift
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Defines
import GI.Gtk.Objects.Box qualified as Gtk
import GI.Gtk.Objects.Separator qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Window qualified as Gtk
import Status.X11.WMStatus (DesktopStat (..))
import System.Applet.Clocks qualified as App
import System.Applet.DesktopVisual qualified as App
import System.Applet.Layout qualified as App
import System.Applet.SysCtrl qualified as App
import System.Applet.SysTray qualified as App
import System.Applet.SystemDisplay qualified as App
import System.Applet.Volume qualified as App
import System.Applet.WMCtrl qualified as App
import System.Environment
import System.Log.LogPrint (LogLevel (..), defLogFormat)
import System.Pulp.PulpBar
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

-- MAYBE Have backend-esque facility

main :: IO ()
main = do
  isTest <- ("test" `elem`) <$> getArgs
  app isTest >>= startPulpApp

app :: Bool -> IO PulpApp
app isTest = do
  dataDir <- getEnv $ if isTest then "XMONAD_CONFIG_DIR" else "XMONAD_DATA_DIR"
  pure
    MkPulpApp
      { pulpArgs =
          MkPulpArg
            { loggerFormat = defLogFormat
            , loggerVerbosity = if isTest then LevelDebug else LevelInfo
            , argDataDir = dataDir
            }
      , pulpAppId = T.pack "pulp.ui.taskbar"
      , pulpBars = [leftBar, centerBar, rightBar]
      , pulpIconDir = "asset" </> "icons"
      , pulpCSSPath = Just ("styles" </> "pulp-taskbar.css")
      }
  where
    dockPos = if isTest then Gtk.DockBottom else Gtk.DockTop

    leftBar =
      MkPulpBar
        { barDockPos = dockPos
        , barDockSize = Gtk.AbsoluteSize 32
        , barDockSpan = Gtk.DockSpan 0 (1 / 6)
        , barTitle = T.pack "Pulp Statusbar"
        , barContent = leftBox
        }
    leftBox :: Gtk.Window -> PulpIO Gtk.Widget
    leftBox win = do
      box <- Gtk.boxNew Gtk.OrientationHorizontal 2
      #setName box (T.pack "pulp-statusbar")
      traverse_ (addToBegin box)
        =<< sequenceA [App.sysCtrlBtn win, App.wmCtrlBtn win]
      traverse_ (addToEnd box)
        =<< sequenceA [App.textClock "%b %_d (%a)\n%H:%M %p"]
      Gtk.toWidget box

    centerBar =
      MkPulpBar
        { barDockPos = dockPos
        , barDockSize = Gtk.AbsoluteSize 36
        , barDockSpan = Gtk.DockSpan (1 / 6) (5 / 6)
        , barTitle = T.pack "Pulp Taskbar"
        , barContent = centerBox
        }
    centerBox :: Gtk.Window -> PulpIO Gtk.Widget
    centerBox _win = do
      box <- Gtk.boxNew Gtk.OrientationHorizontal 0
      #setName box (T.pack "pulp-taskbar")
      #setCenterWidget box . Just =<< App.deskVisualizer deskVisDeskSetup deskVisWinSetup
      traverse_ (addToBegin box)
        =<< sequenceA
          [ App.layout App.LayoutArg{layoutPrettyName = id}
          ]
      traverse_ (addToEnd box)
        =<< sequenceA
          [ App.volumeDisplay "default" "Master"
          , Gtk.toWidget =<< Gtk.separatorNew Gtk.OrientationHorizontal
          , App.mainboardDisplay Gtk.IconSizeLargeToolbar 42
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
            }

    rightBar =
      MkPulpBar
        { barDockPos = dockPos
        , barDockSize = Gtk.AbsoluteSize 32
        , barDockSpan = Gtk.DockSpan (5 / 6) 1
        , barTitle = T.pack "Pulp Systemtray"
        , barContent = rightBox
        }
    rightBox :: Gtk.Window -> PulpIO Gtk.Widget
    rightBox _win = do
      box <- Gtk.boxNew Gtk.OrientationHorizontal 2
      #setName box (T.pack "pulp-systemtray")
      addToEnd box =<< liftIO (App.systemTray sysTrayArgs)
      Gtk.toWidget box
      where
        sysTrayArgs =
          App.SysTrayArgs
            { App.trayOrientation = Gtk.OrientationHorizontal
            , App.trayAlignBegin = False
            }

    addToBegin box wid = Gtk.boxPackStart box wid False False 0
    addToEnd box wid = Gtk.boxPackEnd box wid False False 0
