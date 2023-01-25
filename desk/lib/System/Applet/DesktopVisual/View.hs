{-# LANGUAGE MonoLocalBinds #-}

module System.Applet.DesktopVisual.View (
  widgetUpdateClass,
  module System.Applet.DesktopVisual.DesktopVisual,
  module System.Applet.DesktopVisual.DesktopItemView,
  module System.Applet.DesktopVisual.WindowItemView,
) where

import Control.Monad.IO.Class
import Data.Text qualified as T
import Gtk.Commons qualified as Gtk
import Gtk.Styles qualified as Gtk
import System.Applet.DesktopVisual.DesktopItemView
import System.Applet.DesktopVisual.DesktopVisual
import System.Applet.DesktopVisual.WindowItemView

widgetUpdateClass :: (Gtk.IsWidget w, Enum s, Bounded s, MonadIO m) => w -> (s -> T.Text) -> [s] -> m ()
widgetUpdateClass widget asClass state =
  Gtk.widgetGetStyleContext widget >>= Gtk.updateCssClass asClass state
