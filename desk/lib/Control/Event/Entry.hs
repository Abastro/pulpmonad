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

-- | Temporary solution before phasing out Task
taskToSource :: Task a -> IO (Source a)
taskToSource task = do
  (source, sink) <- sourceSink
  -- Forking for each task to create source is.. not ideal
  forkIO . forever $ do
    val <- taskNextWait task
    sink val
  -- Modifies Source to include the task terminator
  pure . AddHandler $ \handler -> do
    kill <- register source handler
    pure (kill <* taskStop task)
