module Control.Concurrent.Task where

import Control.Concurrent
import Control.Exception
import Control.Exception.Enclosed (tryAny)
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import System.IO

-- | Represents a Task.
data Task a = Task {killTask :: IO (), taskVar :: MVar a}

-- | Starts regular task with delay (ms). Delay should be longer than 10ms.
startRegular :: MonadIO m => Int -> IO a -> m (Maybe (Task a))
startRegular delay action =
  liftIO (try action) >>= \case
    Left (err :: IOException) ->
      Nothing <$ liftIO (hPutStrLn stderr $ "Error while starting task: " <> show err)
    Right val -> liftIO $ do
      var <- newMVar val
      tid <- forkIO . forever $ do
        threadDelay (delay * 1000)
        tryAny action >>= traverse_ (putMVar var)
      pure (Just $ Task (killThread tid) var)
