module UI.X11.DesktopVisual.Handle (
  NumWindows,
  DesktopSetup (..),
  WindowSetup (..),
  deskVisualizer,
) where

import Control.Concurrent.Task
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Foldable
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Traversable
import Data.Tuple
import Data.Vector qualified as V
import Graphics.X11.Types
import Status.X11.WMStatus
import Status.X11.XHandle
import System.Log.Logger
import UI.Commons qualified as UI
import UI.Task qualified as UI
import UI.X11.DesktopVisual.View qualified as View

deskCssClass :: DesktopState -> T.Text
deskCssClass = \case
  DeskActive -> T.pack "active"
  DeskVisible -> T.pack "visible"
  DeskHidden -> T.pack "hidden"

-- Okay, I know, but I cannot resist
winActiveCssClass :: () -> T.Text
winActiveCssClass = \case
  () -> T.pack "active"

windowCssClass :: WMStateEx -> T.Text
windowCssClass = \case
  WinHidden -> T.pack "hidden"
  WinDemandAttention -> T.pack "demanding"

type NumWindows = Word

-- | Desktop part of the setup.
data DesktopSetup = DesktopSetup
  { desktopLabeling :: Maybe T.Text -> T.Text
  -- ^ Labeling rule for the desktop.
  , showDesktop :: DesktopStat -> NumWindows -> Bool
  -- ^ Whether to show certain desktop or not.
  }

-- | Window part of the setup.
data WindowSetup = WindowSetup
  { windowImgSetter :: WindowInfo -> IO View.ImageSet
  -- ^ With which iamge the window is going to set to.
  }

-- | Desktops visualizer widget. Forks its own X11 event handler.
deskVisualizer :: MonadIO m => DesktopSetup -> WindowSetup -> m UI.Widget
deskVisualizer deskSetup winSetup = do
  rcvs <- liftIO $ startXIO deskVisInitiate

  deskVisualView <- View.deskVisualNew
  _ <- liftIO $ deskVisMake rcvs (deskSetup, winSetup) deskVisualView
  UI.widgetShowAll (View.deskVisualWidget deskVisualView)

  pure $ View.deskVisualWidget deskVisualView

data DeskVisModel = DeskVisModel
  { dVisDesks :: !DeskContModel
  , dVisWins :: !WinContModel
  }

-- TODO Model scheme is not working, need to make it work with pure states. Later.
deskVisMake ::
  DeskVisRcvs ->
  (DesktopSetup, WindowSetup) ->
  View.DeskVisual ->
  IO DeskVisModel
deskVisMake DeskVisRcvs{..} (deskSetup, winSetup) view = do
  dVisDesks <- newIORef V.empty
  dVisWins <- newIORef M.empty
  activeWin <- newIORef Nothing
  do
    killStat <- UI.uiTask desktopStats (updateDeskStats dVisDesks)
    killWinCh <- UI.uiTask windowsChange (changeWindows dVisDesks dVisWins)
    killActiv <- UI.uiTask windowActive (changeActivate dVisWins activeWin)
    UI.onWidgetDestroy (View.deskVisualWidget view) (killStat >> killWinCh >> killActiv)

  pure DeskVisModel{..}
  where
    updateDeskStats desksRef newStats = do
      -- Cut down to available desktops
      let numDesk = V.length newStats
      modifyIORef' desksRef (V.take numDesk)
      View.deskVisualCtrl view (View.CutDeskItemsTo numDesk)

      -- Add additional available desktops
      oldNum <- V.length <$> readIORef desksRef
      addeds <- for (V.drop oldNum $ V.indexed newStats) $ \(idx, stat) -> do
        deskItemView <- View.deskItemNew $ reqToDesktop idx
        (deskItemView,) <$> deskItemMake deskSetup stat deskItemView
      modifyIORef' desksRef (<> fmap snd addeds)
      View.deskVisualCtrl view (View.AddDeskItems $ fmap fst addeds)

      -- Update all desktops
      deskItems <- readIORef desksRef
      V.zipWithM (\DeskItemModel{updateDeskItem} -> updateDeskItem) deskItems newStats

    changeWindows desksRef winsRef (DWindowChange _newOrder adding removed) = do
      -- Remove windows
      for_ (S.toList removed) $ \window -> do
        mayItem <- atomicModifyIORef' winsRef $ swap . M.updateLookupWithKey (\_ _ -> Nothing) window
        for_ mayItem $ \WinItemModel{itemWindowView} ->
          UI.widgetDestroy (View.winItemWidget itemWindowView)

      -- Add windows
      for_ (M.toList adding) $ \(window, infoTask) -> do
        existed <- (window `M.member`) <$> readIORef winsRef
        unless existed $ do
          winItemView <- View.winItemNew $ reqActivate window
          winItem <- winItemMake winSetup (winSwitch desksRef window) infoTask winItemView
          modifyIORef' winsRef (M.insert window winItem)

    -- error "Window Reorder"

    changeActivate winsRef activeRef nextActive = do
      windows <- readIORef winsRef
      prevActive <- atomicModifyIORef' activeRef (nextActive,)
      -- Little hack
      for_ (prevActive >>= (windows M.!?)) $ \WinItemModel{itemWindowView} -> do
        View.widgetUpdateClass (View.winItemWidget itemWindowView) winActiveCssClass []
      for_ (nextActive >>= (windows M.!?)) $ \WinItemModel{itemWindowView} -> do
        View.widgetUpdateClass (View.winItemWidget itemWindowView) winActiveCssClass [()]

    winSwitch desksRef windowId winView deskOld deskNew = do
      infoM "DeskVis" (show windowId <> ": Switching desktop from " <> show deskOld <> " to " <> show deskNew)
      desktops <- readIORef desksRef
      -- Out of bounds: was already removed, no need to care
      for_ (desktops V.!? deskOld) $ \DeskItemModel{addRmWinItem} -> addRmWinItem winView False
      -- For now, let's not care about out of bounds from new windows.
      -- That is likely a sync issue, so it needs proper logging later.
      for_ (desktops V.!? deskNew) $ \DeskItemModel{addRmWinItem} -> do
        addRmWinItem winView True

type DeskContModel = IORef (V.Vector DeskItemModel)
data DeskItemModel = DeskItemModel -- Not really a model
  { updateDeskItem :: DesktopStat -> IO ()
  , addRmWinItem :: View.WinItem -> Bool -> IO () -- False for remove, True for add
  }

deskItemMake :: DesktopSetup -> DesktopStat -> View.DeskItem -> IO DeskItemModel
deskItemMake DesktopSetup{..} initStat view = do
  itemDeskStat <- newIORef initStat
  numWinRef <- newIORef 0
  let updateDeskItem = updateItem itemDeskStat numWinRef
  let addRmWinItem = addRmItem itemDeskStat numWinRef
  pure DeskItemModel{..}
  where
    updateItem statRef numWinRef deskStat@DesktopStat{..} = do
      View.deskItemCtrl view (View.DeskLabelName $ desktopLabeling desktopName)
      View.widgetUpdateClass (View.deskItemWidget view) deskCssClass [desktopState]
      writeIORef statRef deskStat
      updateVisible statRef numWinRef

    addRmItem statRef numWinRef winView = \case
      False -> do
        View.deskItemCtrl view (View.RemoveWinItem winView)
        modifyIORef' numWinRef pred
        updateVisible statRef numWinRef
      True -> do
        View.deskItemCtrl view (View.AddWinItems $ V.singleton winView)
        modifyIORef' numWinRef succ
        updateVisible statRef numWinRef

    updateVisible statRef numWinRef = do
      deskStat <- readIORef statRef
      numWindows <- readIORef numWinRef
      case showDesktop deskStat numWindows of
        True -> UI.widgetShowAll (View.deskItemWidget view)
        False -> UI.widgetHide (View.deskItemWidget view)

type WinContModel = IORef (M.Map Window WinItemModel)
data WinItemModel = WinItemModel
  { itemWindowView :: View.WinItem -- As window can move around, its view should be separately owned.
  }

winItemMake ::
  WindowSetup ->
  (View.WinItem -> Int -> Int -> IO ()) ->
  Task WindowInfo ->
  View.WinItem ->
  IO WinItemModel
winItemMake WindowSetup{..} onSwitch infoTask view = do
  itemCurDesktop <- newIORef (-1) -- (-1) is never a valid desktop (means the window is omnipresent)
  do
    killInfo <- UI.uiTask infoTask (updateWindow itemCurDesktop)
    UI.onWidgetDestroy (View.winItemWidget view) killInfo
  let itemWindowView = view
  pure WinItemModel{..}
  where
    updateWindow curDeskRef winInfo@WindowInfo{..} = do
      let newDesk = windowDesktop
      oldDesk <- atomicModifyIORef' curDeskRef $ (newDesk,)
      when (newDesk /= oldDesk) $ onSwitch view oldDesk newDesk

      windowImgSetter winInfo >>= View.winItemSetImg view
      View.widgetUpdateClass (View.winItemWidget view) windowCssClass (S.toList windowState)

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
