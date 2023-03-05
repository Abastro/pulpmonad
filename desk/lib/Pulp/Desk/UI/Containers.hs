module Pulp.Desk.UI.Containers (
  module GI.Gtk.Objects.Container,
  buttonNewWith,
  clickyNewWith,
  overlayed,
) where

import Control.Monad.IO.Class
import Data.Foldable
import GI.Gdk.Structs.EventButton qualified as Gdk
import GI.Gtk.Objects.Button
import GI.Gtk.Objects.Container
import GI.Gtk.Objects.EventBox
import GI.Gtk.Objects.Overlay
import Pulp.Desk.UI.Commons

-- | Button widget.
buttonNewWith :: MonadIO m => Maybe Widget -> IO () -> m Widget
buttonNewWith widget onClick = do
  btn <- buttonNew
  traverse_ (containerAdd btn) widget
  onButtonClicked btn onClick
  toWidget btn

-- | Clickable widget.
clickyNewWith :: MonadIO m => Maybe Widget -> IO () -> m Widget
clickyNewWith widget onClick = do
  clicky <- eventBoxNew
  traverse_ (containerAdd clicky) widget
  onWidgetButtonReleaseEvent clicky $ \btn -> do
    Gdk.getEventButtonButton btn >>= \case
      1 -> True <$ onClick
      _ -> pure False
  toWidget clicky

overlayed :: MonadIO m => Widget -> [Widget] -> m Widget
overlayed core overlays = do
  overlay <- overlayNew
  containerAdd overlay core
  traverse_ (overlayAddOverlay overlay) overlays
  -- Pass through inputs
  traverse_ (\wid -> overlaySetOverlayPassThrough overlay wid True) overlays
  toWidget overlay
