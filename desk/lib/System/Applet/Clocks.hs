{-# LANGUAGE OverloadedLabels #-}

module System.Applet.Clocks (textClock) where

import Control.Event.Entry
import Control.Monad.IO.Class
import Data.GI.Base.Attributes
import Data.Text qualified as T
import Data.Time
import GI.Gtk.Objects.Label qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Task qualified as Gtk
import Reactive.Banana.Frameworks
import System.FilePath
import System.Pulp.PulpPath

-- | Text clock with given format. Queries time every second.
--
-- For format reference, look at 'Data.Time.formatTime' for details.
--
-- Currently button functionality is not implemented.
textClock :: (MonadIO m) => String -> m Gtk.Widget
textClock format = liftIO $ do
  uiFile <- dataPath ("ui" </> "clock.ui")
  View{..} <- view (T.pack uiFile)

  -- To update right away
  network <- compile $ do
    ticker <- sourceEvent (periodicSource 1000)
    time <- pollingBehavior getZonedTime ticker
    syncBehavior time (Gtk.uiSingleRun . setClock . formatted)
  actuate network
  pure clockWidget
  where
    formatted zoned = T.pack $ formatTime defaultTimeLocale format zoned

-- Simple view
data View = View
  { clockWidget :: Gtk.Widget
  , setClock :: Sink T.Text
  }

view :: T.Text -> IO View
view uiFile = Gtk.buildFromFile uiFile $ do
  Just clockLabel <- Gtk.getElement (T.pack "clock-label") Gtk.Label
  Just clockWidget <- Gtk.getElement (T.pack "clock") Gtk.Widget

  let setClock txt = set clockLabel [#label := txt]
  pure View{..}
