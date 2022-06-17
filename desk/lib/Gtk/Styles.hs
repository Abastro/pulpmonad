-- Just because GI-Gtk has weird instance declarations..
{-# LANGUAGE MonoLocalBinds #-}

module Gtk.Styles (
  module GI.Gtk.Objects.StyleContext,
  module GI.Gtk.Objects.CssProvider,
  updateCssClass,
  defScreenAddStyleContext,
) where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Int
import Data.Text qualified as T
import GI.Gdk.Objects.Screen qualified as Gdk
import GI.Gtk.Interfaces.StyleProvider
import GI.Gtk.Objects.CssProvider
import GI.Gtk.Objects.StyleContext

-- | Update CSS class of style context to match the given states.
updateCssClass ::
  (Enum s, Bounded s, MonadIO m) => (s -> T.Text) -> [s] -> StyleContext -> m ()
updateCssClass asClass state ctxt = do
  traverse_ (styleContextRemoveClass ctxt) (asClass <$> [minBound .. maxBound])
  traverse_ (styleContextAddClass ctxt) (asClass <$> state)

-- | Adds style context to the default screen.
defScreenAddStyleContext :: (MonadIO m, IsStyleProvider b) => b -> Int32 -> m ()
defScreenAddStyleContext provider priority =
  Gdk.screenGetDefault
    >>= traverse_
      ( \screen -> styleContextAddProviderForScreen screen provider (fromIntegral priority)
      )
