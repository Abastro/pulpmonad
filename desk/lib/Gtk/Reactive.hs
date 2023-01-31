{-# LANGUAGE GADTs #-}

module Gtk.Reactive (
  onSource,
  activateUI,
  liftMomentIO,
  gtkReact,
  gtkExecute,
  gtkSync,
) where

import Control.Concurrent
import Control.Event.Entry
import Control.Monad.Reader
import Data.GI.Base.BasicTypes
import Data.GI.Base.Signals
import Gtk.Commons
import Gtk.Task qualified as Gtk
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

-- | Connect a signal and take it as a event source.
onSource ::
  (GObject obj, SignalInfo info) =>
  obj ->
  SignalProxy obj info ->
  (Handler a -> HaskellCallbackType info) ->
  Source a
onSource obj proxy asCallback = sourceWithUnreg $ \handler -> do
  handlerId <- on obj proxy (asCallback handler)
  pure (disconnectSignalHandler obj handlerId)

-- | Intended to be used last.
{-# DEPRECATED activateUI "Phasing out" #-}
activateUI :: a -> BuilderM MomentIO () -> BuilderM IO a
activateUI outp desc = do
  network <- mapReaderT compile desc
  -- Should not block main UI from network
  liftIO . forkIO $ actuate network
  pure outp

{-# DEPRECATED liftMomentIO "Phasing out" #-}
liftMomentIO :: MomentIO a -> BuilderM MomentIO a
liftMomentIO = lift

gtkReact :: Event (BuilderM IO ()) -> BuilderM MomentIO ()
gtkReact evt = ReaderT $ \builder -> do
  reactimate $ Gtk.uiSingleRun . (`runReaderT` builder) <$> evt

{-# DEPRECATED gtkExecute "Does not do threading properly for now, do not use" #-}
gtkExecute :: Event (BuilderM MomentIO a) -> BuilderM MomentIO (Event a)
gtkExecute evt = ReaderT $ \builder -> do
  -- Problem: Cannot run "MomentIO" behind syncing action
  -- e.g. sending created UI back to other threads? Channel?
  -- Just not going to work.
  execute $ (`runReaderT` builder) <$> evt

{-# DEPRECATED gtkSync "Phasing out" #-}
gtkSync :: Behavior a -> (a -> BuilderM IO ()) -> BuilderM MomentIO ()
gtkSync behav sink = ReaderT $ \builder -> do
  syncBehavior behav $ Gtk.uiSingleRun . (`runReaderT` builder) . sink
