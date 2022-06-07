module System.Pulp.Applet.Clocks where

import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.Foldable
import Data.Text qualified as T
import Data.Time
import GI.Gtk.Objects.EventBox qualified as UI
import UI.Commons qualified as UI
import UI.Containers qualified as UI
import UI.Task qualified as UI
import View.Textual qualified as View

-- | Text clock with given format. Queries time every second.
--
-- For format reference, look at 'Data.Time.formatTime' for details.
textClock :: MonadIO m => String -> m UI.Widget
textClock format = do
  lbl <- startRegular 1000 getZonedTime >>= traverse clockTxt
  -- Wraps in event box so that it could be empty :P
  ev <- UI.eventBoxNew
  traverse_ (UI.containerAdd ev) (View.labelDynWidget <$> lbl)
  UI.widgetSetName ev (T.pack "clock_text")
  UI.toWidget ev <* UI.widgetShowAll ev
  where
    clockTxt task = do
      label <- View.labelDynNew View.defLabelDyn{View.labelJustify = UI.JustificationCenter}
      liftIO $ do
        kill <- UI.uiTask task $ View.labelDynSetLabel label . T.pack . formatTime defaultTimeLocale format
        UI.onWidgetDestroy (View.labelDynWidget label) kill
      pure label
