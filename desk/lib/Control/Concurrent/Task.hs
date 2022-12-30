module Control.Concurrent.Task (
  Task,
  taskCreate,
  taskStop,
  taskNext,
  taskNextWait,
  startRegular,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import System.IO

-- | Represents a repeatedly run Task. Emits result each time task is run. Can be killed.
data Task a = Task {killTask :: IO (), emitQueue :: TQueue a}

-- | Primitive function to create a task with a given task variable and a way to kill it.
-- killing(stopping) the task should stop the provider from working on the taskVar.
taskCreate :: TQueue a -> IO () -> Task a
taskCreate emitQueue killTask = Task{..}

-- | Stops the task.
-- Should stop the provider from working on the task.
taskStop :: Task a -> IO ()
taskStop Task{killTask} = killTask

-- | Gives the next emitted result if it exists.
taskNext :: Task a -> IO (Maybe a)
taskNext Task{emitQueue} = atomically $ tryReadTQueue emitQueue

-- | Waits until next task is done, and return the emitted result.
-- Can cause deadlock.
taskNextWait :: Task a -> IO a
taskNextWait Task{emitQueue} = atomically $ readTQueue emitQueue

-- Checking first time and yielding None is not a solid logic.
-- Ideally it should be safe to call the action.

-- | Starts regular task with delay (ms).
-- Runs the task once to test its validity.
-- Note that this one only catches IO error. Any other error would be re-thrown.
startRegular :: (MonadIO m) => Int -> IO a -> m (Maybe (Task a))
startRegular delay action = liftIO $ do
  tryIO action >>= \case
    Left err -> do
      hPutStrLn stderr $ "Error while starting task: " <> show err
      pure Nothing
    Right val -> do
      var <- newTQueueIO
      atomically $ writeTQueue var val
      tid <- forkIO . forever $ do
        threadDelay (delay * 1000)
        tryIO action >>= traverse_ (atomically . writeTQueue var)
      pure (Just $ Task{killTask = killThread tid, emitQueue = var})
  where
    tryIO = try @IOException
