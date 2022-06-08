module System.Applet.Clocks where

import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.Default.Class
import Data.Foldable
import Data.Text qualified as T
import Data.Time
import GI.Gtk.Objects.EventBox qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.Task qualified as Gtk
import View.Textual qualified as View

-- | Text clock with given format. Queries time every second.
--
-- For format reference, look at 'Data.Time.formatTime' for details.
textClock :: MonadIO m => String -> m Gtk.Widget
textClock format = do
  lbl <- startRegular 1000 getZonedTime >>= traverse clockTxt
  -- Wraps in event box so that it could be empty :P
  ev <- Gtk.eventBoxNew
  traverse_ (Gtk.containerAdd ev) (View.labelDynWidget <$> lbl)
  Gtk.widgetSetName ev (T.pack "clock_text")
  Gtk.toWidget ev <* Gtk.widgetShowAll ev
  where
    clockTxt task = do
      label <- View.labelDynNew def{View.labelJustify = Gtk.JustificationCenter}
      liftIO $ do
        kill <- Gtk.uiTask task $ View.labelDynSetLabel label . T.pack . formatTime defaultTimeLocale format
        Gtk.onWidgetDestroy (View.labelDynWidget label) kill
      pure label
