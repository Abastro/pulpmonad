module UI.Task where

import Control.Concurrent.MVar
import Data.Foldable
import GI.GLib.Constants
import GI.GLib.Structs.Source
import GI.Gdk.Functions qualified as Gdk
import Task

-- | Adds UI single-use task, which only runs once.
uiSingleRun :: IO a -> IO ()
uiSingleRun task = () <$ Gdk.threadsAddIdle PRIORITY_DEFAULT_IDLE (False <$ task)

-- | Adds UI Task, returns the kill action.
uiTask :: Task a -> (a -> IO b) -> IO (IO ())
uiTask (Task kill var) actWith = do
  let action = True <$ (tryTakeMVar var >>= traverse_ actWith)
  sourceId <- Gdk.threadsAddIdle PRIORITY_DEFAULT_IDLE action
  pure $ kill <* sourceRemove sourceId
