module UI.X11.DesktopVisual.Handle where

import Control.Concurrent.Task
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Vector qualified as V
import Graphics.X11.Types
import Status.X11.WMStatus
import Status.X11.XHandle
import System.Log.Logger
import UI.Commons qualified as UI

deskVisualizer :: MonadIO m => m UI.Widget
deskVisualizer = do
  DeskVisRcvs{..} <- liftIO $ startXIO deskVisInitiate
  
  undefined

data DeskVisModel = DeskVisModel
  { dVisDesks :: !DeskContModel
  , dVisWins :: !WinContModel
  }

type DeskContModel = IORef (V.Vector DeskItemModel)
data DeskItemModel = DeskItemModel
  { itemDesktopIdx :: !Int
  , itemDeskStat :: !(IORef DesktopStat)
  }

type WinContModel = IORef (M.Map Window WinItemModel)
data WinItemModel = WinItemModel
  { itemWindowId :: !Window
  , itemWinInfo :: !(IORef WindowInfo)
  }

{-------------------------------------------------------------------
                          Communication
--------------------------------------------------------------------}

-- | Window changes, with new order, added & removed
data DWindowChange = DWindowChange (M.Map Window Int) (M.Map Window (Task WindowInfo)) (S.Set Window)

-- data DVWindowRcv = DWindowMap Window (Task WindowInfo) | DWindowUnmap Window

data DeskVisRcvs = DeskVisRcvs
  { desktopStats :: Task (V.Vector DesktopStat)
  , windowsChange :: Task DWindowChange
  , windowActive :: Task (Maybe Window)
  , reqActivate :: Window -> IO ()
  , reqToDesktop :: Int -> IO ()
  }

-- | Desktop visualizer event handle initiate.
--
-- The widget holds one entire X event loop,
-- so related resources will be disposed of on kill.
deskVisInitiate :: XIO () DeskVisRcvs
deskVisInitiate = do
  rootWin <- xWindow

  -- Reference to hold current data of windows.
  -- IORef, since X11 handling should be happening on the same thread.
  windowRef <- liftIO $ newIORef S.empty
  -- Detects difference between windows and handle it.
  windowsChange <- errorAct $
    watchXQuery rootWin getAllWindows $ \newWindowsVec -> do
      let order = M.fromList $ zip (V.toList newWindowsVec) [0 ..]
      let newWindows = S.fromList $ V.toList newWindowsVec
      oldWindows <- liftIO $ atomicModifyIORef' windowRef $ \oldWindows -> (newWindows, oldWindows)
      let removed = oldWindows S.\\ newWindows
      let added = newWindows S.\\ oldWindows
      -- "Invalid" windows are ignored from here
      addWins <- M.traverseMaybeWithKey (\_ -> lift . watchWindow) (M.fromSet id added)
      pure $ DWindowChange order addWins removed

  windowActive <- errorAct $ watchXQuery rootWin getActiveWindow pure
  desktopStats <- errorAct $ watchXQuery rootWin getDesktopStat pure
  reqActivate <- reqActiveWindow True
  reqToDesktop <- reqCurrentDesktop

  pure DeskVisRcvs{..}
  where
    onError window err = do
      liftIO (fail $ formatXQError window err)
    errorAct act = do
      window <- xWindow
      act >>= either (onError window) pure
    watchWindow window = do
      watchXQuery window getWindowInfo pure >>= \case
        Left err -> do
          liftIO $ Nothing <$ infoM "DeskVis" ("invalid window: " <> formatXQError window err)
        Right winInfo -> pure (Just winInfo)
