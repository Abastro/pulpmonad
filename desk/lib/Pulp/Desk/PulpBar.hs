{-# LANGUAGE OverloadedLabels #-}

module Pulp.Desk.PulpBar (
  PulpApp (..),
  PulpBar (..),
  startPulpApp,
) where

import Control.Monad
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Data.Foldable
import Data.GI.Base
import Data.Text qualified as T
import GI.GLib.Constants qualified as Glib
import GI.GLib.Functions qualified as Glib
import GI.Gio.Flags qualified as Gio
import GI.Gtk.Constants qualified as Gtk
import GI.Gtk.Objects.Application qualified as Gtk
import GI.Gtk.Objects.IconTheme qualified as Gtk
import GI.Gtk.Objects.Widget qualified as Gtk
import Pulp.Desk.Env.PulpEnv
import Pulp.Desk.PulpPath
import Pulp.Desk.UI.Commons qualified as Gtk
import Pulp.Desk.UI.Styles qualified as Gtk
import Pulp.Desk.UI.Window qualified as Gtk
import System.Exit (ExitCode (..), exitWith)
import System.Posix.Signals qualified as Sig

-- | For pulpbar application.
data PulpApp = MkPulpApp
  { pulpArgs :: !PulpArg
  , pulpAppId :: !T.Text
  , pulpBars :: [PulpBar]
  , pulpIconDir :: FilePath
  -- ^ Icon directory to search for, relative to data directory
  , pulpCSSPath :: Maybe FilePath
  -- ^ CSS file to load, relative to the data directory
  }

startPulpApp :: PulpApp -> IO ()
startPulpApp MkPulpApp{..} = runPulpIO pulpArgs $ withRunInIO $ \unlift -> do
  Just app <- Gtk.applicationNew (Just pulpAppId) [Gio.ApplicationFlagsNonUnique]
  on app #activate (unlift $ activating app)
  Glib.unixSignalAdd Glib.PRIORITY_DEFAULT (fromIntegral Sig.sigINT) $ True <$ #quit app
  status <- #run app Nothing
  when (status /= 0) $ exitWith (ExitFailure $ fromIntegral status)
  where
    activating app = withRunInIO $ \unlift -> do
      -- Prepare
      traverse_ loadCSS pulpCSSPath
      appendIconTheme pulpIconDir

      -- Spawns windows
      traverse_ (#showAll <=< unlift . barWindow app) pulpBars

    loadCSS :: FilePath -> IO ()
    loadCSS cssPath = do
      css <- new Gtk.CssProvider []
      dataDir <- dataPath cssPath
      #loadFromPath css (T.pack dataDir)
      Gtk.defScreenAddStyleContext css Gtk.STYLE_PROVIDER_PRIORITY_USER

    appendIconTheme :: FilePath -> IO ()
    appendIconTheme iconDir = do
      defaultTheme <- Gtk.iconThemeGetDefault
      iconDirAt <- dataPath iconDir
      #appendSearchPath defaultTheme iconDirAt

-- | For each bar window.
data PulpBar = MkPulpBar
  { barDockPos :: !Gtk.DockPos
  , barDockSize :: !Gtk.DockSize
  , barDockSpan :: !Gtk.DockSpan
  , barTitle :: !T.Text
  , barContent :: Gtk.Window -> PulpIO Gtk.Widget
  }

-- | Creates and links the taskbar window. Does not show the window.
barWindow :: Gtk.Application -> PulpBar -> PulpIO Gtk.Window
barWindow app MkPulpBar{..} = do
  window <-
    new
      Gtk.Window
      [ #type := Gtk.WindowTypeToplevel
      , #title := barTitle
      ]
  #setKeepBelow window True
  Gtk.windowSetDock window $ Gtk.DockArg barDockPos barDockSize barDockSpan Nothing
  Gtk.windowSetTransparent window

  content <- barContent window
  #add window content
  #getStyleContext content >>= flip #addClass (T.pack "pulp-bar")

  #addWindow app window
  pure window
