module Control.Event.Entry (
  Source,
  Sink,
  Handler,
  sourceSimple,
  sourceWithUnreg,
  sourceSink,
  sourceEvent,
  syncBehavior,
  taskToSource,
  periodicSource,
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

-- | Sync with behavior changes using given sink.
syncBehavior :: Behavior a -> Sink a -> MomentIO ()
syncBehavior behav sink = do
  chEvent <- changes behav
  reactimate' (fmap sink <$> chEvent)

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

-- Problem: The looping source have to use time for calling callbacks.
-- This might lead to delay issues.

-- | Simple periodic source with given period (in millisecond).
periodicSource :: Int -> IO (Source ())
periodicSource period = loopSource (threadDelay $ period * 1000) (pure ())
