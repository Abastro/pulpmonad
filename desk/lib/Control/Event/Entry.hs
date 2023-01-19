module Control.Event.Entry (
  Source,
  Sink,
  Handler,
  sourceSimple,
  sourceWithUnreg,
  sourceSink,
  sourceEvent,
  sourceBehavior,
  syncBehavior,
  syncBehaviorDiff,
  taskToSource,
  taskToBehavior,
  periodicSource,
  pollingBehavior,
  pollingBehaviorWithEvent,
) where

import Control.Concurrent
import Control.Concurrent.Task
import Control.Event.Handler
import Control.Monad
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

-- | Source receives a callback, and send the data to callback when signal occurs.
type Source a = AddHandler a

-- | Sink is, effectively, a callback.
type Sink a = Handler a

-- | Source with capability of unregistering handlers.
sourceWithUnreg :: (Handler a -> IO (IO ())) -> Source a
sourceWithUnreg = AddHandler

-- | Source without capability of unregistering handlers.
sourceSimple :: (Handler a -> IO ()) -> Source a
sourceSimple src = sourceWithUnreg $ \handler -> pure () <$ src handler

-- | Source paired with sink to call.
sourceSink :: IO (Source a, Sink a)
sourceSink = newAddHandler

sourceEvent :: Source a -> MomentIO (Event a)
sourceEvent = fromAddHandler

sourceBehavior :: a -> Source a -> MomentIO (Behavior a)
sourceBehavior = fromChanges

-- | Sync with behavior using given sink.
syncBehavior :: Behavior a -> Sink a -> MomentIO ()
syncBehavior behav sink = do
  -- Reflects initial value
  initial <- valueBLater behav
  liftIOLater $ sink initial
  chEvent <- changes behav
  reactimate' (fmap sink <$> chEvent)

-- | Sync with behavior using given sink, which takes both older and newer values.
--
-- Since older value does not exist on startup, sync is not performed then.
syncBehaviorDiff :: Behavior a -> (a -> a -> IO ()) -> MomentIO ()
syncBehaviorDiff behav sink = do
  chEvent <- changes behav
  reactimate' (sinkFuture <$> behav <@> chEvent)
  where
    sinkFuture old newFuture = sink old <$> newFuture

-- | Looping source from performing action with cleanup.
loopSource :: IO a -> IO () -> IO (Source a)
loopSource act cleanup = do
  (source, sink) <- sourceSink
  tid <- forkIO . forever $ act >>= sink
  -- Modifies source to include the cleanup.
  pure . AddHandler $ \handler -> do
    kill <- register source handler
    pure (kill <* cleanup <* killThread tid)

-- | Temporary solution before phasing out Task.
taskToSource :: Task a -> IO (Source a)
taskToSource task = loopSource (taskNextWait task) (taskStop task)

-- Do we need taskToBehavior?

-- | Waits for first task to finish, so that we get behavior
taskToBehavior :: Task a -> MomentIO (Behavior a)
taskToBehavior task = do
  init <- liftIO (taskNextWait task)
  src <- liftIO (taskToSource task)
  sourceBehavior init src

-- Potential Problem: The looping source have to use time for calling callbacks.
-- This might lead to delay issues.
-- Reactimates might better run tasks on other threads.

-- | Simple periodic source with given period (in millisecond).
periodicSource :: Int -> IO (Source ())
periodicSource period = loopSource (threadDelay $ period * 1000) (pure ())

-- | Polling behavior which updates at each event.
pollingBehavior :: IO a -> Event b -> MomentIO (Behavior a)
pollingBehavior act evt = snd <$> pollingBehaviorWithEvent act evt

-- | Polling behavior with event for the updates of the behavior.
pollingBehaviorWithEvent :: IO a -> Event b -> MomentIO (Event a, Behavior a)
pollingBehaviorWithEvent act evt = do
  first <- liftIO act
  updates <- mapEventIO (const act) evt
  behav <- stepper first updates
  pure (updates, behav)
