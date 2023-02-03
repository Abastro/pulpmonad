{-# LANGUAGE RecursiveDo #-}

module Control.Event.Entry (
  Source,
  Sink,
  Discrete,
  sourceSimple,
  sourceWithUnreg,
  sourceSink,
  sourceEvent,
  sourceBehavior,
  diffEvent,
  syncBehavior,
  loopSource,
  taskToSource,
  taskToBehavior,
  periodicSource,
  pollingBehavior,
  pollingDiscrete,
) where

import Control.Concurrent
import Control.Concurrent.Task
import Control.Event.Handler
import Control.Monad
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import System.Mem.Weak (addFinalizer)

-- | Source receives a callback, and send the data to callback when signal occurs.
type Source a = AddHandler a

-- | Sink is, effectively, a callback.
type Sink a = Handler a

-- | Discrete behavior paired with its own update event.
type Discrete a = (Event a, Behavior a)

-- | Source with capability of unregistering handlers.
--
-- Returned action is supposed to unregister the handler.
sourceWithUnreg :: (Handler a -> IO (IO ())) -> Source a
sourceWithUnreg = AddHandler

-- | Source without capability of unregistering handlers.
sourceSimple :: (Handler a -> IO ()) -> Source a
sourceSimple src = sourceWithUnreg $ \handler -> pure () <$ src handler

-- | Source paired with sink to call.
sourceSink :: IO (Source a, Sink a)
sourceSink = newAddHandler

-- FIXME Reactive-banana does not call "unregister" of AddHandler on GC.
-- Unregistering action by finalizer might not be reliable..
sourceEvent :: Source a -> MomentIO (Event a)
sourceEvent src = do
  rec let handledSrc = AddHandler $ \handler -> do
            unregister <- register src handler
            -- Here, since unregister action is never called.
            addFinalizer event unregister
            pure unregister
      event <- fromAddHandler handledSrc
  pure event

sourceBehavior :: a -> Source a -> MomentIO (Behavior a)
sourceBehavior = fromChanges

-- | Compute difference between old and new value of a behaviors on each change.
diffEvent ::
  (a -> a -> b) ->
  Behavior a ->
  MomentIO (Event (Future b))
diffEvent compute bOrigin = do
  eChange <- changes bOrigin
  pure $ liftedCompute <$> bOrigin <@> eChange
  where
    liftedCompute old = fmap (compute old)

-- | Sync with behavior using given sink.
syncBehavior :: Behavior a -> Sink a -> MomentIO ()
syncBehavior behav sink = do
  -- Reflects initial value
  initial <- valueBLater behav
  liftIOLater $ sink initial
  chEvent <- changes behav
  reactimate' (fmap sink <$> chEvent)

-- | Looping source from performing action with cleanup.
-- Forks a thread on each register call, so each handler would receive calls separately.
loopSource :: IO a -> IO () -> Source a
loopSource act cleanup = AddHandler $ \handler -> do
  tid <- forkIO . forever $ act >>= handler
  pure (cleanup <> killThread tid)

-- | Temporary solution before phasing out Task.
taskToSource :: Task a -> Source a
taskToSource task = loopSource (taskNextWait task) (taskStop task)

-- | Waits for first task to finish, so that we get a behavior.
taskToBehavior :: Task a -> MomentIO (Behavior a)
taskToBehavior task = do
  init <- liftIO (taskNextWait task)
  sourceBehavior init (taskToSource task)

-- Potential Problem: The looping source have to use time for calling callbacks.
-- This might lead to delay issues.
-- Reactimates might better run tasks on other threads.

-- | Simple periodic source with given period (in millisecond).
periodicSource :: Int -> Source ()
periodicSource period = loopSource (threadDelay $ period * 1000) (pure ())

-- | Polling behavior which updates at each event.
pollingBehavior :: IO a -> Event b -> MomentIO (Behavior a)
pollingBehavior act evt = snd <$> pollingDiscrete act evt

-- | Polling discrete behavior which updates at each event.
pollingDiscrete :: IO a -> Event b -> MomentIO (Discrete a)
pollingDiscrete act evt = do
  first <- liftIO act
  updates <- mapEventIO (const act) evt
  behav <- stepper first updates
  pure (updates, behav)
