{-# LANGUAGE OverloadedLabels #-}

module System.Applet.Clocks where

import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.Foldable
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.Text qualified as T
import Data.Time
import GI.Gtk.Objects.EventBox qualified as Gtk
import GI.Gtk.Objects.Label qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Task qualified as Gtk

-- | Text clock with given format. Queries time every second.
--
-- For format reference, look at 'Data.Time.formatTime' for details.
textClock :: MonadIO m => String -> m Gtk.Widget
textClock format = do
  lbl <- startRegular 1000 getZonedTime >>= traverse clockTxt
  -- Wraps in event box so that it could be empty :P
  ev <- new Gtk.EventBox []
  traverse_ (#add ev) lbl
  #setName ev (T.pack "clock-text")
  Gtk.toWidget ev <* #showAll ev
  where
    clockTxt task = do
      label <- new Gtk.Label [#justify := Gtk.JustificationCenter]
      liftIO $ do
        kill <- Gtk.uiTask task (setLabel label)
        Gtk.onWidgetDestroy label kill
      Gtk.toWidget label

    setLabel wid lbl = set wid [#label := T.pack $ formatTime defaultTimeLocale format lbl]
