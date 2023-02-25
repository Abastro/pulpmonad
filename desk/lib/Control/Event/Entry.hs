{-# LANGUAGE RecursiveDo #-}

module Control.Event.Entry (
  Source,
  Sink,
  Discrete,
  sourceSimple,
  sourceWithUnreg,
  sourceSink,
  sourceWaitTill,
  sourceEvent,
  sourceEventWA,
  diffEvent,
  exeMapAccum,
  exeAccumD,
  syncBehavior,
  reactEvent,
  loopSource,
  taskToSource,
  taskToBehavior,
  taskToBehaviorWA,
  periodicSource,
  pollingBehavior,
  pollingDiscrete,
) where

import Control.Concurrent
import Control.Concurrent.Task
import Control.Event.Handler
import Control.Monad
import Data.IORef
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

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

-- | Modified source that waits until the given 'MVar' is set to emit new value.
sourceWaitTill :: MVar () -> Source a -> Source a
sourceWaitTill wait src = AddHandler $ \handler -> do
  register src $ \val -> do
    () <- readMVar wait
    handler val

-- FIXME Reactive-banana does not call "unregister" of AddHandler on GC.
-- Unregistering action by finalizer is not reliable.

sourceEvent :: Source a -> MomentIO (Event a)
sourceEvent = fromAddHandler

-- | Workaround for sourceEvent to unregister handler properly.
sourceEventWA :: Source a -> MomentIO (Event a, IO ())
sourceEventWA src = do
  ref <- liftIO . newIORef $ pure ()
  event <- sourceEvent (wrappedSource ref)
  pure (event, join $ readIORef ref)
  where
    wrappedSource ref = AddHandler $ \handler -> do
      unregister <- register src handler
      modifyIORef' ref (<> unregister)
      pure $ pure ()

-- We do not need sourceBehavior, easy enough to create Behavior

-- MAYBE diffEvent should accept `a -> Future a -> b` for more descriptive types

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

-- | mapAccum which accumulates by executing momentous action.
--
-- WARNING: Either the event or the behavior needs to be used.
--
-- If they are discarded, it will invoke GC and result in unpredicted behavior.
--
-- NOTE: reactive-banana's `execute` call holds a lock to network, preventing any update.
exeMapAccum :: acc -> Event (acc -> MomentIO (sig, acc)) -> MomentIO (Event sig, Behavior acc)
exeMapAccum initial eFn = do
  rec bAcc <- stepper initial eAcc
      eSigAcc <- execute (newSigAcc <$> bAcc <@> eFn)
      let eSig = fst <$> eSigAcc
          eAcc = snd <$> eSigAcc
  pure (eSig, bAcc)
  where
    newSigAcc acc update = acc `seq` update acc

-- | Discrete behavior which accumulates by executing momentous action.
--
-- Inherits the same caveat as 'exeMapAccum'.
exeAccumD :: a -> Event (a -> MomentIO a) -> MomentIO (Discrete a)
exeAccumD initial eFn = do
  exeMapAccum initial (withSig <$> eFn)
  where
    both x = (x, x)
    withSig fn = fmap both . fn

-- | Sync with behavior using given sink.
syncBehavior :: Behavior a -> Sink a -> MomentIO ()
syncBehavior behav sink = do
  -- Reflects initial value
  initial <- valueBLater behav
  liftIOLater $ sink initial
  chEvent <- changes behav
  reactimate' (fmap sink <$> chEvent)

-- | React based on event, returning freeing action.
--
-- Freeing does not remove the reactimate itself,
-- but allows the action itself to be discarded with event.
reactEvent :: Event (IO ()) -> MomentIO (IO ())
reactEvent event = do
  (eFree, free) <- newEvent
  -- Hopefully this allows eFree to be freed.
  eFreeOnce <- once eFree
  eLimited <- switchE event (never <$ eFreeOnce)
  reactimate eLimited
  -- Forks to avoid deadlock when called inside `execute`.
  pure (void $ forkIO $ free ())

-- | Looping source from performing action with cleanup.
-- Forks a thread on each register call, so each handler would receive calls separately.
loopSource :: IO a -> IO () -> Source a
loopSource act cleanup = sourceWithUnreg $ \handler -> do
  tid <- forkIO . forever $ act >>= handler
  pure (cleanup <> killThread tid)

-- | Temporary solution before phasing out Task.
--
-- A task should not be shared between many sources - This need to be fixed later.
taskToSource :: Task a -> Source a
taskToSource task = loopSource task.emit task.stop

-- | Waits for first task to finish, so that we get a behavior.
taskToBehavior :: Task a -> MomentIO (Behavior a)
taskToBehavior task = do
  init <- liftIO task.emit
  eTask <- sourceEvent (taskToSource task)
  stepper init eTask

taskToBehaviorWA :: Task a -> MomentIO (Behavior a, IO ())
taskToBehaviorWA task = do
  init <- liftIO task.emit
  (eTask, unreg) <- sourceEventWA (taskToSource task)
  (,unreg) <$> stepper init eTask

-- | Simple periodic source with given period (in millisecond).
periodicSource :: Int -> Source ()
periodicSource period = loopSource (threadDelay $ period * 1000) (pure ())

-- | Polling behavior which updates at each event.
pollingBehavior :: IO a -> Event b -> MomentIO (Behavior a)
pollingBehavior act evt = snd <$> pollingDiscrete act evt

-- | Polling discrete behavior which updates at each event.
-- It is not guaranteed that result event is simultaneous with input event.
pollingDiscrete :: IO a -> Event b -> MomentIO (Discrete a)
pollingDiscrete act evt = do
  first <- liftIO act
  updates <- mapEventIO (const act) evt
  behav <- stepper first updates
  pure (updates, behav)
