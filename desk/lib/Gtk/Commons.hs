{-# LANGUAGE OverloadedLabels #-}

module Gtk.Commons (
  module GI.Gtk.Enums,
  module GI.Gdk.Enums,
  module GI.Gtk.Flags,
  module GI.Gtk.Functions,
  module GI.Gdk.Flags,
  module GI.Gtk.Constants,
  module GI.Gdk.Constants,
  module GI.Gtk.Objects.Widget,
  withClassAs,
  templateChild,
  BuilderM,
  buildFromFile,
  addCallback,
  addCallbackWithEvent,
  getElement,
) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.GI.Base.BasicTypes
import Data.GI.Base.GObject
import Data.GI.Base.ManagedPtr
import Data.Text qualified as T
import Foreign.Ptr (nullPtr)
import GI.Gdk.Constants hiding (MAJOR_VERSION, MICRO_VERSION, MINOR_VERSION)
import GI.Gdk.Enums hiding (AnotherWindowType, WindowType, WindowTypeToplevel)
import GI.Gdk.Flags
import GI.Gdk.Unions.Event
import GI.Gtk.Constants hiding (MAJOR_VERSION, MICRO_VERSION, MINOR_VERSION)
import GI.Gtk.Enums
import GI.Gtk.Flags
import GI.Gtk.Functions
import GI.Gtk.Objects.Builder
import GI.Gtk.Objects.Widget

withClassAs :: (ManagedPtr a -> a) -> GObjectClass -> (a -> IO b) -> IO b
withClassAs constr gClass act = withTransient (coerce gClass) (act . constr)

templateChild :: forall o. (GObject o) => Widget -> T.Text -> (ManagedPtr o -> o) -> IO o
templateChild widget name constr = do
  childType <- glibType @o
  asObj <- #getTemplateChild widget childType name
  unsafeCastTo constr asObj

-- | Monad with builder attached
type BuilderM m = ReaderT Builder m

buildFromFile :: MonadIO m => T.Text -> BuilderM m a -> m a
buildFromFile uiFile act = do
  builder <- builderNewFromFile uiFile
  built <- runReaderT act builder
  -- Finalizes signal connection
  builderConnectSignals builder nullPtr
  pure built

-- | Adds callback to a signal.
addCallback :: MonadIO m => T.Text -> IO () -> BuilderM m ()
addCallback name act = ReaderT $ \builder -> do
  builderAddCallbackSymbol builder name act

-- | Adds callback with event.
--
-- The casting is unsafe, due to how gi-gtk is currently implemented.
-- (It does not provide support for handling unions)
addCallbackWithEvent :: (MonadIO m) => T.Text -> (Event -> IO o) -> (o -> IO ()) -> BuilderM m ()
addCallbackWithEvent name specify act = addCallback name $ void . runMaybeT $ do
  general <- MaybeT getCurrentEvent
  lift $ specify general >>= act

-- | Gets an element in GtkBuilder.
getElement :: (GObject o, MonadIO m) => T.Text -> (ManagedPtr o -> o) -> BuilderM m (Maybe o)
getElement name constr = ReaderT $ \builder -> runMaybeT $ do
  obj <- MaybeT $ builderGetObject builder name
  MaybeT . liftIO $ castTo constr obj
