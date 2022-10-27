module Gtk.Reactive where

import Control.Monad.Reader
import Data.Text qualified as T
import GI.Gdk.Unions.Event qualified as Gdk
import Gtk.Commons
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Control.Event.Entry
import Control.Concurrent

-- | Intended to be used last.
activateUI :: a -> BuilderM MomentIO () -> BuilderM IO a
activateUI outp desc = do
  network <- mapReaderT compile desc
  -- Should not block main UI from network
  liftIO . forkIO $ actuate network
  pure outp

liftMomentIO :: MomentIO a -> BuilderM MomentIO a
liftMomentIO = lift

gtkEvent :: T.Text -> BuilderM MomentIO (Event Gdk.Event)
gtkEvent name = ReaderT $ \builder -> do
  let source handler = runReaderT (addCallbackWithEvent name handler) builder
  fromAddHandler $ sourceSimple source
