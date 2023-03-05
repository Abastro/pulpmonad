module Pulp.Desk.UI.Task (
  uiSingleRun,
  uiCreate,
) where

import Control.Concurrent.MVar
import Control.Monad
import GI.GLib.Constants
import GI.Gdk.Functions qualified as Gdk

-- | Adds UI single-use task, which only runs once.
uiSingleRun :: IO a -> IO ()
uiSingleRun task = void $ Gdk.threadsAddIdle PRIORITY_DEFAULT_IDLE (False <$ task)

-- | Creates using the action in UI thread and returns it through MVar.
--
-- Please do not put values into the MVar.
--
-- Should not be called from main thread in most cases.
uiCreate :: IO a -> IO (MVar a)
uiCreate make = do
  -- TODO Check if / Guard against main thread?
  res <- newEmptyMVar
  uiSingleRun $ make >>= putMVar res
  pure res
