module Gtk.Commons (
  module GI.Gtk.Enums
  , module GI.Gdk.Enums
  , module GI.Gtk.Flags
  , module GI.Gdk.Flags
  , module GI.Gtk.Constants
  , module GI.Gdk.Constants
  , module GI.Gtk.Objects.Widget
  , module GI.Gtk.Objects.Builder
  , elementAs
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.GI.Base.BasicTypes
import Data.GI.Base.ManagedPtr
import Data.Text qualified as T
import GI.Gdk.Constants hiding (MAJOR_VERSION, MICRO_VERSION, MINOR_VERSION)
import GI.Gdk.Enums hiding (AnotherWindowType, WindowType, WindowTypeToplevel)
import GI.Gdk.Flags
import GI.Gtk.Constants hiding (MAJOR_VERSION, MICRO_VERSION, MINOR_VERSION)
import GI.Gtk.Enums
import GI.Gtk.Flags
import GI.Gtk.Objects.Builder
import GI.Gtk.Objects.Widget

-- | Get an element in a GtkBuilder as a type.
elementAs :: (MonadIO m, GObject o) => Builder -> T.Text -> (ManagedPtr o -> o) -> m (Maybe o)
elementAs builder name constr = runMaybeT $ do
  obj <- MaybeT $ builderGetObject builder name
  MaybeT . liftIO $ castTo constr obj
