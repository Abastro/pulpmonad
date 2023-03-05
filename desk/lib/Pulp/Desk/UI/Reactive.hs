{-# LANGUAGE GADTs #-}

module Pulp.Desk.UI.Reactive (
  onSource,
  activateUI,
  liftMomentIO,
  gtkReact,
  gtkExecute,
  gtkSync,
) where

import Control.Concurrent
import Control.Monad.Reader
import Data.GI.Base.BasicTypes
import Data.GI.Base.Signals
import Pulp.Desk.Reactive.Entry
import Pulp.Desk.UI.Commons
import Pulp.Desk.UI.Task
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

-- MAYBE Forks a new thread on every signal handling.

-- | Connect a signal and take it as a event source.
--
-- Connecting and disconnecting is both done in the UI thread.
--
-- Caveat: Can deadlock if Source is unregistered in UI thread while UI is being created.
onSource ::
  (GObject obj, SignalInfo info) =>
  obj ->
  SignalProxy obj info ->
  (Handler a -> HaskellCallbackType info) ->
  Source a
onSource obj proxy asCallback = sourceWithUnreg $ \handler -> do
  varHandlerId <- uiCreate $ on obj proxy (asCallback handler)
  pure $ do
    handlerId <- takeMVar varHandlerId
    uiSingleRun $ disconnectSignalHandler obj handlerId

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
  reactimate $ uiSingleRun . (`runReaderT` builder) <$> evt

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
  syncBehavior behav $ uiSingleRun . (`runReaderT` builder) . sink
