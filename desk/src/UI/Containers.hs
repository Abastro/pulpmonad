module UI.Containers (
  module GI.Gtk.Objects.Container,
  buttonNewWith,
  boxed,
  homogBoxed,
  overlayed,
) where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Int
import GI.Gtk.Objects.Box
import GI.Gtk.Objects.Button
import GI.Gtk.Objects.Container
import GI.Gtk.Objects.Overlay
import UI.Commons

buttonNewWith :: MonadIO m => Maybe Widget -> IO () -> m Widget
buttonNewWith widget onClick = do
  btn <- buttonNew
  traverse_ (containerAdd btn) widget
  onButtonClicked btn onClick
  toWidget btn

boxed :: MonadIO m => Orientation -> Int32 -> [Widget] -> m Widget
boxed orient spacing children = do
  box <- boxNew orient spacing
  traverse_ (containerAdd box) children
  toWidget box

homogBoxed :: MonadIO m => Orientation -> Int32 -> [Widget] -> m Widget
homogBoxed orient spacing children = do
  box <- boxNew orient spacing
  traverse_ (containerAdd box) children
  boxSetHomogeneous box True
  toWidget box

overlayed :: MonadIO m => Widget -> [Widget] -> m Widget
overlayed core overlays = do
  overlay <- overlayNew
  containerAdd overlay core
  traverse_ (overlayAddOverlay overlay) overlays
  -- Pass through inputs
  traverse_ (\wid -> overlaySetOverlayPassThrough overlay wid True) overlays
  toWidget overlay
