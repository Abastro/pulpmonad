module Control.Concurrent.Task where

import Control.Concurrent
import Control.Exception
import Control.Exception.Enclosed (tryAny)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Foldable
import System.IO

-- | Represents a repeatedly run Task.
-- The taskVar needs to be put out repeatedly, unless, the task would not resume.
data Task a = Task {killTask :: IO (), taskVar :: MVar a}

-- | Task with a repeater and initial value provided.
startWithRepeater ::
  (MonadIO m, MonadUnliftIO m) =>
  ((r -> m ()) -> m ()) ->
  a ->
  (r -> m (Maybe a)) ->
  m (Task a)
startWithRepeater repeater initial nextAct = withRunInIO $ \unlifts -> do
  var <- newMVar initial
  tid <- forkIO . unlifts . repeater $ \inp ->
    nextAct inp >>= traverse_ (liftIO . putMVar var)
  pure (Task{killTask = killThread tid, taskVar = var})

-- | Starts regular task with delay (ms).
startRegular :: (MonadIO m) => Int -> IO a -> m (Maybe (Task a))
startRegular delay action = liftIO $ do
  try action >>= \case
    Left (err :: IOException) -> do
      hPutStrLn stderr $ "Error while starting task: " <> show err
      pure Nothing
    Right val -> do
      var <- newMVar val
      tid <- forkIO . forever $ do
        threadDelay (delay * 1000)
        tryAny action >>= traverse_ (putMVar var)
      pure (Just $ Task{killTask = killThread tid, taskVar = var})
