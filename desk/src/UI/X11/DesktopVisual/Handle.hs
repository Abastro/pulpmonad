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
import Data.List
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
import Control.Monad.Except

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

-- Currently handle is empty, because no external handling is permitted.
data DeskVisHandle = DeskVisHandle

-- TODO Model scheme is not working, need to make it work with pure states. Later.
deskVisMake ::
  DeskVisRcvs ->
  (DesktopSetup, WindowSetup) ->
  View.DeskVisual ->
  IO DeskVisHandle
deskVisMake DeskVisRcvs{..} (deskSetup, winSetup) view = do
  join $ registers <$> newIORef V.empty <*> newIORef M.empty <*> newIORef M.empty <*> newIORef Nothing
  where
    registers desksRef winsRef ordersRef activeRef = do
      killStat <- UI.uiTask desktopStats updateDeskStats
      killWinCh <- UI.uiTask windowsChange changeWindows
      killActiv <- UI.uiTask windowActive changeActivate
      _ <- UI.onWidgetDestroy (View.deskVisualWidget view) (killStat >> killWinCh >> killActiv)
      pure DeskVisHandle
      where
        updateDeskStats newStats = do
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
          V.zipWithM (\DeskItemHandle{updateDeskItem} -> updateDeskItem) deskItems newStats

        changeWindows (DWindowChange newOrder adding removed) = do
          -- Remove windows
          for_ (S.toList removed) $ \window -> do
            mayItem <- atomicModifyIORef' winsRef $ swap . M.updateLookupWithKey (\_ _ -> Nothing) window
            for_ mayItem $ \WinItemHandle{itemWindowView} ->
              UI.widgetDestroy (View.winItemWidget itemWindowView)

          -- Add windows
          for_ (M.toList adding) $ \(window, (winDeskTask, infoTask)) -> do
            existed <- (window `M.member`) <$> readIORef winsRef
            unless existed $ do
              winItemView <- View.winItemNew $ reqActivate window
              winItem <- winItemMake winSetup (winSwitch desksRef window) winDeskTask infoTask winItemView
              modifyIORef' winsRef (M.insert window winItem)

          -- Reorder windows appropriately
          writeIORef ordersRef newOrder
          curWins <- readIORef winsRef
          desktops <- readIORef desksRef
          for_ desktops $ \DeskItemHandle{reorderWinItems} -> reorderWinItems (newOrder M.!?) curWins

        changeActivate nextActive = do
          windows <- readIORef winsRef
          prevActive <- atomicModifyIORef' activeRef (nextActive,)
          -- Little hack
          for_ (prevActive >>= (windows M.!?)) $ \WinItemHandle{itemWindowView} -> do
            View.widgetUpdateClass (View.winItemWidget itemWindowView) winActiveCssClass []
          for_ (nextActive >>= (windows M.!?)) $ \WinItemHandle{itemWindowView} -> do
            View.widgetUpdateClass (View.winItemWidget itemWindowView) winActiveCssClass [()]

        winSwitch desksRef windowId winView deskOld deskNew = do
          infoM "DeskVis" (show windowId <> ": Switching desktop from " <> show deskOld <> " to " <> show deskNew)
          desktops <- readIORef desksRef
          curOrders <- readIORef ordersRef
          -- Out of bounds: was already removed, no need to care
          for_ (desktops V.!? deskOld) $ \DeskItemHandle{addRmWinItem} -> do
            addRmWinItem winView windowId WinRemove
          -- For now, let's not care about out of bounds from new windows.
          -- That is likely a sync issue, so it needs proper logging later.
          for_ (desktops V.!? deskNew) $ \DeskItemHandle{addRmWinItem} -> do
            addRmWinItem winView windowId (WinAdd (curOrders M.!?))

-- WinAdd parameter is for ordering
data DeskWinMod = WinRemove | WinAdd (Window -> Maybe Int)
data DeskItemHandle = DeskItemHandle
  { updateDeskItem :: DesktopStat -> IO ()
  , addRmWinItem :: View.WinItem -> Window -> DeskWinMod -> IO () -- False for remove, True for add
  , reorderWinItems :: (Window -> Maybe Int) -> M.Map Window WinItemHandle -> IO ()
  }

deskItemMake :: DesktopSetup -> DesktopStat -> View.DeskItem -> IO DeskItemHandle
deskItemMake DesktopSetup{..} initStat view = do
  creates <$> newIORef initStat <*> newIORef V.empty
  where
    creates statRef curWindows = DeskItemHandle{..}
      where
        updateDeskItem deskStat@DesktopStat{..} = do
          View.deskItemCtrl view (View.DeskLabelName $ desktopLabeling desktopName)
          View.widgetUpdateClass (View.deskItemWidget view) deskCssClass [desktopState]
          writeIORef statRef deskStat
          updateVisible curWindows

        addRmWinItem winView windowId = \case
          WinRemove -> do
            View.deskItemCtrl view (View.RemoveWinItem winView)
            modifyIORef' curWindows (V.filter (/= windowId))
            updateVisible curWindows
          WinAdd curOrd -> do
            (prev, next) <- V.span (\w -> curOrd w < curOrd windowId) <$> readIORef curWindows
            writeIORef curWindows (prev <> V.singleton windowId <> next)
            View.deskItemCtrl view (View.AddWinItemAt winView $ V.length prev)
            updateVisible curWindows

        reorderWinItems newOrd winItems = do
          olds <- readIORef curWindows
          let news = V.fromList . sortOn newOrd . V.toList $ olds
          when (news /= olds) $ do
            writeIORef curWindows news
            let newViewOrd =
                  (\WinItemHandle{itemWindowView} -> itemWindowView) <$> V.mapMaybe (winItems M.!?) news
            View.deskItemCtrl view (View.ReorderWinItems newViewOrd)

        updateVisible curWindows = do
          deskStat <- readIORef statRef
          numWindows <- fromIntegral . V.length <$> readIORef curWindows
          case showDesktop deskStat numWindows of
            True -> UI.widgetShowAll (View.deskItemWidget view)
            False -> UI.widgetHide (View.deskItemWidget view)

data WinItemHandle = WinItemHandle
  { itemWindowView :: View.WinItem -- As window can move around, its view should be separately owned.
  }

winItemMake ::
  WindowSetup ->
  (View.WinItem -> Int -> Int -> IO ()) ->
  Task Int ->
  Task WindowInfo ->
  View.WinItem ->
  IO WinItemHandle
winItemMake WindowSetup{..} onSwitch winDeskTask infoTask view = do
  -- (-1) is never a valid desktop (means the window is omnipresent)
  join $ registers <$> newIORef (-1)
  where
    registers curDeskRef = do
      killChDesk <- UI.uiTask winDeskTask changeDesktop
      killInfo <- UI.uiTask infoTask updateWindow
      UI.onWidgetDestroy (View.winItemWidget view) (killChDesk >> killInfo)
      pure WinItemHandle{itemWindowView = view}
      where
        changeDesktop newDesk = do
          oldDesk <- atomicModifyIORef' curDeskRef $ (newDesk,)
          when (newDesk /= oldDesk) $ onSwitch view oldDesk newDesk

        updateWindow winInfo@WindowInfo{..} = do
          windowImgSetter winInfo >>= View.winItemSetImg view
          View.widgetUpdateClass (View.winItemWidget view) windowCssClass (S.toList windowState)

{-------------------------------------------------------------------
                          Communication
--------------------------------------------------------------------}

-- | Window changes, with new order, added & removed
data DWindowChange
  = DWindowChange
      (M.Map Window Int)
      (M.Map Window (Task Int, Task WindowInfo))
      (S.Set Window)

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
  -- Exists because each new window needs their own tasks.
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
      excRes <- runExceptT $ do
        winDesk <- ExceptT $ watchXQuery window getWindowDesktop pure
        winInfo <- ExceptT $ watchXQuery window getWindowInfo pure
        pure (winDesk, winInfo)
      case excRes of
        Left err -> do
          liftIO $ Nothing <$ infoM "DeskVis" ("invalid window: " <> formatXQError window err)
        Right res -> pure (Just res)