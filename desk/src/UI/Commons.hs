{-# LANGUAGE MonoLocalBinds #-}

module UI.Commons (
  module GI.Gtk.Enums,
  module GI.Gdk.Enums,
  module GI.Gtk.Constants,
  module GI.Gdk.Constants,
  module GI.Gtk.Objects.Widget,
  module GI.Gtk.Objects.StyleContext,
  defscreenAddStyleContext,
) where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Int
import GI.Gdk.Constants hiding (MAJOR_VERSION, MICRO_VERSION, MINOR_VERSION)
import GI.Gdk.Enums hiding (AnotherWindowType, WindowType, WindowTypeToplevel)
import GI.Gdk.Objects.Screen
import GI.Gtk.Constants hiding (MAJOR_VERSION, MICRO_VERSION, MINOR_VERSION)
import GI.Gtk.Enums
import GI.Gtk.Interfaces.StyleProvider
import GI.Gtk.Objects.StyleContext
import GI.Gtk.Objects.Widget

-- TODO
-- Taffybar is suboptimal, esp. dependency. Cook one up myself?

defscreenAddStyleContext :: (MonadIO m, IsStyleProvider b) => b -> Int32 -> m ()
defscreenAddStyleContext provider priority =
  screenGetDefault
    >>= traverse_
      ( \screen -> styleContextAddProviderForScreen screen provider (fromIntegral priority)
      )
