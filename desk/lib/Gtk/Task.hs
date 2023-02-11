module Gtk.Task (uiSingleRun, uiCreate, uiTask) where

import Control.Concurrent.MVar
import Control.Concurrent.Task
import Control.Monad
import Data.Foldable
import GI.GLib.Constants
import GI.GLib.Structs.Source
import GI.Gdk.Functions qualified as Gdk

-- | Adds UI single-use task, which only runs once.
uiSingleRun :: IO a -> IO ()
uiSingleRun task = void $ Gdk.threadsAddIdle PRIORITY_DEFAULT_IDLE (False <$ task)

-- | Creates using the action and returns it through MVar.
-- Please do not put values into the MVar.
uiCreate :: IO a -> IO (MVar a)
uiCreate make = do
  res <- newEmptyMVar
  uiSingleRun $ make >>= putMVar res
  pure res

-- NOTE: Currently, `uiTask` is the sole consumer of Task.
-- Likely could be replaced with something else.

-- | Adds UI Task, returns the kill action. Each UI task is checked every 10ms.
-- WARNING: a task should not be given to 2 UI tasks.
uiTask :: Task a -> (a -> IO b) -> IO (IO ())
uiTask task actWith = do
  let action = True <$ (taskNext task >>= traverse_ actWith)
  sourceId <- Gdk.threadsAddTimeout PRIORITY_DEFAULT 10 action
  pure $ taskStop task <* sourceRemove sourceId
