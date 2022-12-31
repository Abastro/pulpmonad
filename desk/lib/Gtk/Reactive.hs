module Gtk.Reactive where

import Control.Concurrent
import Control.Event.Entry
import Control.Monad.Reader
import Gtk.Commons
import Gtk.Task qualified as Gtk
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

-- | Intended to be used last.
activateUI :: a -> BuilderM MomentIO () -> BuilderM IO a
activateUI outp desc = do
  network <- mapReaderT compile desc
  -- Should not block main UI from network
  liftIO . forkIO $ actuate network
  pure outp

liftMomentIO :: MomentIO a -> BuilderM MomentIO a
liftMomentIO = lift

{-
gtkEvent :: T.Text -> BuilderM MomentIO (Event Gdk.Event)
gtkEvent name = ReaderT $ \builder -> do
  let source handler = runReaderT (addCallbackWithEvent name handler) builder
  fromAddHandler $ sourceSimple source
-}

gtkReact :: Event (BuilderM IO ()) -> BuilderM MomentIO ()
gtkReact evt = ReaderT $ \builder -> do
  reactimate $ Gtk.uiSingleRun . (`runReaderT` builder) <$> evt

{-# DEPRECATED gtkExecute "Does not do threading properly for now, do not use" #-}
gtkExecute :: Event (BuilderM MomentIO a) -> BuilderM MomentIO (Event a)
gtkExecute evt = ReaderT $ \builder -> do
  -- Problem: Cannot run "MomentIO" behind syncing action
  -- e.g. sending created UI back to other threads? Channel?
  execute $ (`runReaderT` builder) <$> evt

gtkSync :: Behavior a -> (a -> BuilderM IO ()) -> BuilderM MomentIO ()
gtkSync behav sink = ReaderT $ \builder -> do
  syncBehavior behav $ Gtk.uiSingleRun . (`runReaderT` builder) . sink
