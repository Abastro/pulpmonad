module Gtk.Commons (
  module GI.Gtk.Enums
  , module GI.Gdk.Enums
  , module GI.Gtk.Flags
  , module GI.Gdk.Flags
  , module GI.Gtk.Constants
  , module GI.Gdk.Constants
  , module GI.Gtk.Objects.Widget
  , BuilderIO
  , buildFromFile
  , addCallback
  , getElement
) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.GI.Base.BasicTypes
import Data.GI.Base.ManagedPtr
import Data.Text qualified as T
import Foreign.Ptr (nullPtr)
import GI.Gdk.Constants hiding (MAJOR_VERSION, MICRO_VERSION, MINOR_VERSION)
import GI.Gdk.Enums hiding (AnotherWindowType, WindowType, WindowTypeToplevel)
import GI.Gdk.Flags
import GI.Gtk.Constants hiding (MAJOR_VERSION, MICRO_VERSION, MINOR_VERSION)
import GI.Gtk.Enums
import GI.Gtk.Flags
import GI.Gtk.Objects.Builder
import GI.Gtk.Objects.Widget

type BuilderIO = ReaderT Builder IO

buildFromFile :: T.Text -> BuilderIO a -> IO a
buildFromFile uiFile act = do
  builder <- builderNewFromFile uiFile
  built <- runReaderT act builder
  -- Finalizes signal connection
  builderConnectSignals builder nullPtr
  pure built

-- | Adds callback to a signal.
addCallback :: T.Text -> IO () -> BuilderIO ()
addCallback name act = ReaderT $ \builder -> do
  builderAddCallbackSymbol builder name act

-- | Gets an element in GtkBuilder.
getElement :: GObject o => T.Text -> (ManagedPtr o -> o) -> BuilderIO (Maybe o)
getElement name constr = ReaderT $ \builder -> runMaybeT $ do
  obj <- MaybeT $ builderGetObject builder name
  MaybeT . liftIO $ castTo constr obj
