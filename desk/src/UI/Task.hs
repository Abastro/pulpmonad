module UI.Task where

import Data.Foldable
import GI.GLib.Constants
import GI.GLib.Structs.Source
import GI.Gdk.Functions qualified as Gdk
import Control.Concurrent.Task

-- | Adds UI single-use task, which only runs once.
uiSingleRun :: IO a -> IO ()
uiSingleRun task = () <$ Gdk.threadsAddIdle PRIORITY_DEFAULT_IDLE (False <$ task)

-- | Adds UI Task, returns the kill action. Each UI task is checked every 1ms.
-- WARNING: a task should not be given to 2 UI tasks.
uiTask :: Task a -> (a -> IO b) -> IO (IO ())
uiTask task actWith = do
  let action = True <$ (taskNext task >>= traverse_ actWith)
  sourceId <- Gdk.threadsAddTimeout PRIORITY_DEFAULT_IDLE 1 action
  pure $ taskStop task <* sourceRemove sourceId
