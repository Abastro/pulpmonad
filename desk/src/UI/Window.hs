module UI.Window (
  appWindowNew,
  windowAsTransparent,
  windowGrabOnMap,
  module GI.Gtk.Objects.Window,
) where

import Control.Monad
import Control.Monad.IO.Class
import GI.Gdk.Flags qualified as Gdk
import GI.Gdk.Objects.Cursor qualified as Gdk
import GI.Gdk.Objects.Display qualified as Gdk
import GI.Gdk.Objects.Screen qualified as Gdk
import GI.Gdk.Objects.Seat qualified as Gdk
import GI.Gdk.Objects.Window qualified as Gdk
import GI.Gdk.Structs.EventAny qualified as Gdk
import GI.Gtk.Functions
import GI.Gtk.Objects.Application
import GI.Gtk.Objects.ApplicationWindow
import GI.Gtk.Objects.Window
import UI.Commons

-- | Create window for an application.
appWindowNew :: MonadIO m => Application -> m Window
appWindowNew app = applicationWindowNew app >>= toWindow

-- | Make window as transparent.
windowAsTransparent :: MonadIO m => Window -> m ()
windowAsTransparent window = do
  setWidgetAppPaintable window True
  screen <- windowGetScreen window
  composited <- Gdk.screenIsComposited screen
  when composited $
    Gdk.screenGetRgbaVisual screen >>= widgetSetVisual window

-- | Grab the screen on window map
windowGrabOnMap :: MonadIO m => Window -> m ()
windowGrabOnMap window = do
  afterWidgetMapEvent window $
    Gdk.getEventAnyWindow >=> \case
      Nothing -> pure False
      Just win -> do
        event <- getCurrentEvent
        seat <- Gdk.windowGetDisplay win >>= Gdk.displayGetDefaultSeat
        Gdk.seatGrab seat win [Gdk.SeatCapabilitiesAll] True (Nothing @Gdk.Cursor) event Nothing
        pure False
  pure ()
