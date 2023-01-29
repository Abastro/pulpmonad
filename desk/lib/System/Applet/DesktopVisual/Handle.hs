{-# LANGUAGE RecursiveDo #-}

module System.Applet.DesktopVisual.Handle (
  NumWindows,
  GetXIcon,
  DesktopSetup (..),
  WindowSetup (..),
  deskVisualizer,
) where

import Control.Applicative
import Control.Concurrent.Task
import Control.Event.Entry
import Control.Event.State
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Maybe
import Data.Bifunctor (Bifunctor (..), first)
import Data.Foldable
import Data.Function
import Data.IntMap.Strict qualified as IM
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import GI.GLib qualified as Glib
import GI.Gio.Interfaces.AppInfo qualified as Gio
import GI.Gio.Interfaces.Icon qualified as Gio
import Graphics.X11.Types
import Gtk.Commons qualified as Gtk
import Gtk.Pixbufs qualified as Gtk
import Gtk.Task qualified as Gtk
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Status.AppInfos
import Status.X11.WMStatus
import Status.X11.XHandle
import System.Applet.DesktopVisual.DesktopItemView qualified as View
import System.Applet.DesktopVisual.DesktopVisual qualified as View
import System.Applet.DesktopVisual.WindowItemView qualified as View
import System.Log.LogPrint

type NumWindows = Word

-- TODO Consider that in XMonad, "Workspace" has String ID.

-- | Desktop part of the setup.
data DesktopSetup = DesktopSetup
  { desktopLabeling :: Maybe T.Text -> T.Text
  -- ^ Labeling rule for the desktop.
  , showDesktop :: DesktopStat -> NumWindows -> Bool
  -- ^ Whether to show certain desktop or not.
  }

-- | Window part of the setup.
newtype WindowSetup = WindowSetup
  { windowImgIcon :: WindowInfo -> MaybeT IO Gio.Icon
  -- ^ With which icon the window is going to set to.
  -- Due to implementation concerns, icon would not be updated on title changes.
  }

deskVisualizer ::
  (MonadUnliftIO m, MonadLog m, MonadXHand m) =>
  DesktopSetup ->
  WindowSetup ->
  m Gtk.Widget
deskVisualizer deskSetup winSetup = withRunInIO $ \unlift -> do
  DeskVisRcvs{..} <- unlift $ runXHand deskVisInitiate
  mainView <- Glib.new View.DesktopVisual []

  compile $ do
    eDesktopList <- liftIO (taskToSource desktopStats) >>= sourceEvent
    eWindowList <- liftIO (taskToSource windowsList) >>= sourceEvent
    eActiveWindow <- liftIO (taskToSource windowActive) >>= sourceEvent

    bDesktops <- desktopList (updateDesktop deskSetup (Glib.new View.DesktopItemView [])) eDesktopList
    (eWindows, bWinWithDesk) <- windowMap (updateWindow (Glib.new View.WindowItemView []) trackWinInfo) eWindowList
    bActiveWindow <- stepper Nothing eActiveWindow
    let deskViews = V.map desktopView <$> bDesktops
        winDeskPairs = windowDesktopPairs <$> bDesktops <*> bWinWithDesk

    -- Compute and apply container differences.
    deskDiffs <- updateEvent computeDiffs deskViews deskViews
    pairDiffs <- updateEvent computeDiffs winDeskPairs winDeskPairs
    reactimate' $ fmap @Future (Gtk.uiSingleRun . applyDeskDiff mainView) <$> deskDiffs
    reactimate' $ fmap @Future (Gtk.uiSingleRun . applyPairDiff) <$> pairDiffs

    -- Consequence of window list update.
    reactimate $ Gtk.uiSingleRun <$> (applyWindowPriority <$> bDesktops <@> eWindows)

    -- Visibility depends on both desktop state and window count.
    let deskWithWinCnt = pairDeskCnt <$> winDeskPairs <*> bDesktops
    syncBehavior deskWithWinCnt (Gtk.uiSingleRun . traverse_ (uncurry $ applyDesktopVisible deskSetup))

  Gtk.toWidget mainView
  where
    pairDeskCnt pairMap = V.imap $ \i v -> (fromMaybe 0 (deskHisto IM.!? i), v)
      where
        pairToCnt (_win, desk) = (desk, 1 :: NumWindows)
        deskHisto = IM.fromListWith (+) . map pairToCnt $ M.keys pairMap

data DeskItem = DeskItem
  { desktopView :: !View.DesktopItemView
  , deskItemStat :: !DesktopStat
  }

data WinItemOf a = WinItem
  { windowView :: !View.WindowItemView
  , winIndex :: !Int
  -- ^ Index in the window list.
  , winItemExtra :: !a
  -- ^ Extra data to carry around signals / desktops.
  }
  deriving (Functor, Foldable, Traversable)

type WinItem = WinItemOf ()
type WinWithDesk = WinItemOf Int
type WinItemFull = WinItemOf (Behavior Int)

type ViewPair = (View.WindowItemView, View.DesktopItemView)

windowDesktopPairs ::
  V.Vector DeskItem ->
  M.Map Window WinWithDesk ->
  M.Map (Window, Int) ViewPair
windowDesktopPairs desktops windows = M.mapMaybe pairToView . M.fromSet id $ pairSet
  where
    -- Problem: Having to reference view this way is unsatisfactory.
    -- Better way of carrying around view?
    pairToView (win, desk) = (winViewOf win,) <$> deskViewOf desk
    deskViewOf desk = desktopView <$> desktops V.!? desk
    winViewOf win = windowView $ windows M.! win

    pairSet = S.fromList . map winHandleToPair . M.toList $ windows
    winHandleToPair (window, WinItem{winItemExtra}) = (window, winItemExtra)

-- Problem: Looks generalizable, while presence IO action complicates the matter.
-- Remove first and Add in order, this part might need to be considered.
applyDeskDiff :: View.DesktopVisual -> Diffs (V.Vector View.DesktopItemView) -> IO ()
applyDeskDiff main Diffs{..} = do
  -- Remove first, then add "in order".
  traverse_ (View.removeDesktop main) removed
  traverse_ (View.addDesktop main) added

applyPairDiff :: Diffs (M.Map k ViewPair) -> IO ()
applyPairDiff Diffs{..} = do
  traverse_ (uncurry . flip $ View.removeWindow) removed
  traverse_ (uncurry . flip $ View.insertWindow) added

-- TODO Need a test that same ID points towards the same View. Likely require restructure.
desktopList ::
  (Maybe DeskItem -> DesktopStat -> MomentIO DeskItem) ->
  Event (V.Vector DesktopStat) ->
  MomentIO (Behavior (V.Vector DeskItem))
desktopList update eStat = do
  (_, bList) <- exeAccumD V.empty (flip (zipToRightM update) <$> eStat)
  pure bList

updateDesktop ::
  DesktopSetup ->
  IO View.DesktopItemView ->
  Maybe DeskItem ->
  DesktopStat ->
  MomentIO DeskItem
updateDesktop setup mkView old deskItemStat = do
  desktop <- maybe createDesktop pure old
  liftIO $ applyDesktopState setup desktop
  pure desktop
  where
    createDesktop = do
      desktopView <- liftIO mkView
      pure DeskItem{..}

windowMap ::
  (Maybe WinItemFull -> (Window, Int) -> MomentIO (Maybe WinItemFull)) ->
  Event (V.Vector Window) ->
  MomentIO (Event (M.Map Window WinItem), Behavior (M.Map Window WinWithDesk))
windowMap update eList = do
  let eWinIdx = asMapWithIdx <$> eList
  (eMap, _) <- exeAccumD M.empty (flip (filterZipToRightM update) <$> eWinIdx)
  let eStdMap = M.map void <$> eMap
      eDeskMap = traverse sequenceA <$> eMap
  bDeskMap <- switchB (pure M.empty) eDeskMap
  pure (eStdMap, bDeskMap)
  where
    asMapWithIdx list = M.mapWithKey (,) . M.fromList $ zip (V.toList list) [0 ..]

updateWindow ::
  IO View.WindowItemView ->
  (Window -> IO (Maybe PerWinRcvs)) ->
  Maybe WinItemFull ->
  (Window, Int) ->
  MomentIO (Maybe WinItemFull)
updateWindow mkView trackInfo old (windowId, winIndex) = runMaybeT $ updated <|> createWindow
  where
    createWindow = do
      PerWinRcvs{..} <- MaybeT . liftIO $ trackInfo windowId
      windowView <- liftIO mkView
      lift $ do
        winItemExtra <- taskToBehavior winDesktop
        winItemInfo <- taskToBehavior winInfo
        -- Apply the received information
        syncBehavior winItemInfo (applyWindowState windowView)
        -- TODO Apply Icon - how?

        pure WinItem{..}
    updated = MaybeT $ pure (setIndex <$> old)
    setIndex window = window{winIndex}

-- | Update priority of windows, then reflect the priority.
applyWindowPriority :: V.Vector DeskItem -> M.Map k WinItem -> IO ()
applyWindowPriority desktops windows = do
  -- MAYBE Too complex? Priority might be embedded in view..
  for_ windows $ \WinItem{..} -> View.setPriority windowView winIndex
  for_ desktops $ \DeskItem{desktopView} -> View.reflectPriority desktopView

applyDesktopState :: DesktopSetup -> DeskItem -> IO ()
applyDesktopState DesktopSetup{..} (DeskItem desktop DesktopStat{..}) = do
  View.desktopSetLabel desktop (desktopLabeling desktopName)
  View.desktopSetState desktop (viewState desktopState)
  where
    viewState = \case
      DeskActive -> View.DesktopActive
      DeskVisible -> View.DesktopVisible
      DeskHidden -> View.DesktopHidden

applyDesktopVisible :: DesktopSetup -> NumWindows -> DeskItem -> IO ()
applyDesktopVisible DesktopSetup{..} numWin (DeskItem desktop stat) = do
  View.desktopSetVisible desktop (showDesktop stat numWin)

applyWindowState :: View.WindowItemView -> WindowInfo -> IO ()
applyWindowState view WindowInfo{..} = do
  Gtk.widgetSetTooltipText view (Just windowTitle)
  View.windowSetStates view (map viewState $ S.toList windowState)
  where
    viewState = \case
      WinHidden -> View.WindowHidden
      WinDemandAttention -> View.WindowDemanding

applyWindowIcon ::
  (MonadUnliftIO m, MonadLog m) =>
  WindowSetup ->
  AppInfoCol ->
  GetXIcon ->
  View.WindowItemView ->
  WindowInfo ->
  m ()
applyWindowIcon WindowSetup{..} appCol getXIcon view winInfo = withRunInIO $ \unlift -> do
  -- TODO Prevent unnecessary updates
  -- AppInfo case: Update when classes change
  -- Other case: Update when classes or states change
  -- .. Or watch for icons?
  (unlift . runMaybeT) imgIcon >>= \case
    Just gicon -> View.windowSetGIcon view gicon
    Nothing -> do
      rawIcons <- getXIcon >>= unlift . handleError
      View.windowSetRawIcons view rawIcons
  where
    WindowInfo{windowClasses} = winInfo
    imgIcon = appInfoImgSetter appCol windowClasses <|> mapMaybeT liftIO (windowImgIcon winInfo)
    handleError = \case
      Left err -> [] <$ logS (T.pack "DeskVis") LevelDebug (logStrf "Cannot recognize icon due to: $1" err)
      Right icons -> pure icons

{-
deskVisMake ::
  (MonadUnliftIO m, MonadLog m) =>
  DeskVisRcvs ->
  (DesktopSetup, WindowSetup) ->
  View.DesktopVisual ->
  m DeskVisHandle
deskVisMake DeskVisRcvs{..} (deskSetup, winSetup) view = withRunInIO $ \unlift -> do
  act <- registers <$> trackAppInfo <*> newIORef V.empty <*> newIORef M.empty <*> newIORef Nothing
  unlift act
  where
    registers appCol desksRef winsRef activeRef = withRunInIO $ \unlift -> do
      killStat <- Gtk.uiTask desktopStats (unlift . updateDeskStats)
      killWinCh <- Gtk.uiTask windowsList (unlift . updateWinList)
      killActiv <- Gtk.uiTask windowActive (unlift . changeActivate)
      _ <- Gtk.onWidgetDestroy view (killStat >> killWinCh >> killActiv)
      pure DeskVisHandle
      where
        updateDeskStats :: (MonadUnliftIO m, MonadLog m) => V.Vector DesktopStat -> m ()
        updateDeskStats newStats = withRunInIO $ \unlift -> do
          -- Cut down to available desktops
          let numDesk = V.length newStats
          modifyIORef' desksRef (V.take numDesk)
          View.cutDesktopToCount view numDesk

          -- Add additional available desktops
          oldNum <- V.length <$> readIORef desksRef
          addeds <- for (V.drop oldNum $ V.indexed newStats) $ \(idx, stat) -> do
            deskItemView <- Glib.new View.DesktopItemView []
            View.desktopClickAct deskItemView $ reqToDesktop idx
            (deskItemView,) <$> unlift (deskItemMake deskSetup stat deskItemView)
          modifyIORef' desksRef (<> fmap snd addeds)
          View.addDesktops view (fst <$> addeds)

          -- Update all desktops
          deskItems <- readIORef desksRef
          V.zipWithM_ (\DeskItemHandle{updateDeskItem} -> updateDeskItem) deskItems newStats

        removeOldWin _window WinItemHandle{itemWindowDestroy} = do
          False <$ liftIO itemWindowDestroy

        addNewWin window () = withRunInIO $ \unlift ->
          trackWinInfo window >>= \case
            Just winRcvs -> do
              winItemView <- Glib.new View.WindowItemView []
              View.windowClickAct winItemView $ reqActivate window
              let getIcon = winGetIcon window
              let switcher item x y = unlift (winSwitch window item x y)
              unlift $ Just <$> winItemMake winSetup appCol getIcon switcher winRcvs winItemView
            Nothing -> pure Nothing

        updateWinList :: (MonadUnliftIO m, MonadLog m) => V.Vector Window -> m ()
        updateWinList windows = withRunInIO $ \unlift -> do
          -- Update window maps
          oldWinMap <- readIORef winsRef
          let newWinMap = M.fromSet (const ()) . S.fromList . V.toList $ windows
          updWinMap <-
            unlift $
              M.mergeA
                (M.filterAMissing removeOldWin)
                (M.traverseMaybeMissing addNewWin)
                (M.zipWithMatched $ \_ handle _ -> handle)
                oldWinMap
                newWinMap
          writeIORef winsRef updWinMap

          -- Apply window order through priority
          let newPriority = M.fromList (V.toList windows `zip` [0 ..])
          curWins <- readIORef winsRef
          desktops <- readIORef desksRef
          for_ (M.toList curWins) $ \(window, WinItemHandle{itemWindowView}) -> do
            View.setPriority itemWindowView (newPriority M.! window)
          for_ desktops $ \DeskItemHandle{reorderWinItems} -> reorderWinItems

        changeActivate :: (MonadUnliftIO m, MonadLog m) => Maybe Window -> m ()
        changeActivate nextActive = withRunInIO $ \_unlift -> do
          windows <- readIORef winsRef
          prevActive <- atomicModifyIORef' activeRef (nextActive,)
          for_ (prevActive >>= (windows M.!?)) $ \WinItemHandle{itemWindowView} -> do
            View.windowSetActivate itemWindowView False
          for_ (nextActive >>= (windows M.!?)) $ \WinItemHandle{itemWindowView} -> do
            View.windowSetActivate itemWindowView True

        winSwitch :: (MonadUnliftIO m, MonadLog m) => Window -> View.WindowItemView -> Int -> Int -> m ()
        winSwitch windowId winView deskOld deskNew = withRunInIO $ \unlift -> do
          unlift $ logS (T.pack "DeskVis") LevelDebug $ logStrf "[$1] desktop $2 -> $3" windowId deskOld deskNew
          desktops <- readIORef desksRef
          -- Out of bounds: was already removed, no need to care
          for_ (desktops V.!? deskOld) $ \DeskItemHandle{addRmWinItem} -> do
            addRmWinItem winView WinRemove
          -- For now, let's not care about out of bounds from new windows.
          -- That is likely a sync issue, so it needs proper logging later.
          for_ (desktops V.!? deskNew) $ \DeskItemHandle{addRmWinItem} -> do
            addRmWinItem winView WinAdd

data DeskWinMod = WinRemove | WinAdd
data DeskItemHandle = DeskItemHandle
  { updateDeskItem :: DesktopStat -> IO ()
  , addRmWinItem :: View.WindowItemView -> DeskWinMod -> IO ()
  , reorderWinItems :: IO ()
  }

deskItemMake :: MonadIO m => DesktopSetup -> DesktopStat -> View.DesktopItemView -> m DeskItemHandle
deskItemMake DesktopSetup{..} initStat view = liftIO $ do
  creates <$> newIORef initStat <*> newIORef 0
  where
    creates statRef numWindows = DeskItemHandle{..}
      where
        updateDeskItem deskStat@DesktopStat{..} = do
          View.desktopSetLabel view (desktopLabeling desktopName)
          View.desktopSetState view (viewState desktopState)
          writeIORef statRef deskStat
          updateVisible

        addRmWinItem winView = \case
          WinRemove -> do
            View.removeWindow view winView
            modifyIORef' numWindows pred
            updateVisible
          WinAdd -> do
            View.insertWindow view winView
            modifyIORef' numWindows succ
            updateVisible

        reorderWinItems = View.reflectPriority view

        updateVisible = do
          deskStat <- readIORef statRef
          numWindows <- readIORef numWindows
          View.desktopSetVisible view (showDesktop deskStat numWindows)

        viewState = \case
          DeskActive -> View.DesktopActive
          DeskVisible -> View.DesktopVisible
          DeskHidden -> View.DesktopHidden

data WinItemHandle = WinItemHandle
  { itemWindowView :: View.WindowItemView -- As window can move around, its view should be separately owned.
  , itemWindowDestroy :: IO ()
  }

winItemMake ::
  (MonadUnliftIO m, MonadLog m) =>
  WindowSetup ->
  AppInfoCol ->
  GetXIcon ->
  (View.WindowItemView -> Int -> Int -> IO ()) ->
  PerWinRcvs ->
  View.WindowItemView ->
  m WinItemHandle
winItemMake WindowSetup{..} appCol getXIcon onSwitch PerWinRcvs{..} view = withRunInIO $ \unlift -> do
  -- (-1) is never a valid desktop (means the window is omnipresent)
  act <- registers <$> newIORef (-1) <*> newIORef 0
  unlift act
  where
    imgIcon winInfo = appInfoImgSetter appCol winInfo <|> mapMaybeT liftIO (windowImgIcon winInfo)

    registers curDeskRef lastUpRef = withRunInIO $ \unlift -> do
      killChDesk <- Gtk.uiTask winDesktop changeDesktop
      killInfo <- Gtk.uiTask winInfo (unlift . updateWindow)
      -- Reference will only be dropped when the parent widget gets destroyed.
      -- In that case, program close should take care of resource reclaim.
      pure WinItemHandle{itemWindowView = view, itemWindowDestroy = destroyWindow (killChDesk >> killInfo)}
      where
        destroyWindow freeAct = do
          freeAct -- Stop receiving updates
          changeDesktop (-1) -- Frees from desktops
          -- Release the object (replacing widgetDestroy)
          Glib.releaseObject view

        changeDesktop newDesk = do
          oldDesk <- atomicModifyIORef' curDeskRef (newDesk,)
          when (newDesk /= oldDesk) $ onSwitch view oldDesk newDesk

        updateWindow winInfo@WindowInfo{..} = withRunInIO $ \unlift -> do
          -- Limites update to once per second
          updTime <- getPOSIXTime
          diff <- atomicModifyIORef' lastUpRef $ \oldTime -> (updTime, updTime - oldTime)
          when (diff > 1) $ do
            Gtk.widgetSetTooltipText view (Just windowTitle)
            -- TODO When the icon is decided by X property,
            -- receive updates solely from that property (Forgot, does it mean getXIcon?)
            (unlift . runMaybeT) (imgIcon winInfo) >>= \case
              Just gicon -> View.windowSetGIcon view gicon
              Nothing -> do
                rawIcons <-
                  getXIcon >>= \case
                    Left err -> unlift $ do
                      -- MAYBE Window id?
                      [] <$ logS (T.pack "DeskVis") LevelDebug (logStrf "Cannot recognize icon due to: $1" err)
                    Right icons -> pure icons
                View.windowSetRawIcons view rawIcons
            View.windowSetStates view (map viewState $ S.toList windowState)

        viewState = \case
          WinHidden -> View.WindowHidden
          WinDemandAttention -> View.WindowDemanding
-}

{-------------------------------------------------------------------
                          Application Info
--------------------------------------------------------------------}

appInfoImgSetter :: (MonadUnliftIO m, MonadLog m) => AppInfoCol -> V.Vector T.Text -> MaybeT m Gio.Icon
appInfoImgSetter appCol classes = do
  allDat <- liftIO $ getAppInfos appCol
  let findWith matcher = V.find (\dat -> any (matcher dat) classes) allDat
  appDat@AppInfoData{appId} <- MaybeT . pure $ findWith classMatch <|> findWith identMatch <|> findWith execMatch
  lift $ logS (T.pack "DeskVis") LevelDebug $ logStrf "AppInfo: $1 -> $2" (show classes) appId
  appInfo <- MaybeT . liftIO $ appGetIns appDat
  MaybeT $ Gio.appInfoGetIcon appInfo
  where
    classMatch AppInfoData{..} cl = Just cl == appWmClass
    identMatch AppInfoData{..} cl = T.toLower cl <> T.pack ".desktop" == T.toLower appId
    execMatch AppInfoData{..} cl = Just (T.toLower cl) == appExecName

{-------------------------------------------------------------------
                          Communication
--------------------------------------------------------------------}

type GetXIcon = IO (Either String [Gtk.RawIcon])

data DeskVisRcvs = DeskVisRcvs
  { desktopStats :: Task (V.Vector DesktopStat)
  , windowsList :: Task (V.Vector Window)
  , windowActive :: Task (Maybe Window)
  , trackWinInfo :: Window -> IO (Maybe PerWinRcvs)
  , reqActivate :: Window -> IO ()
  , reqToDesktop :: Int -> IO ()
  , winGetIcon :: Window -> GetXIcon
  }

data PerWinRcvs = PerWinRcvs
  { winDesktop :: !(Task Int)
  , winInfo :: !(Task WindowInfo)
  }

-- | Desktop visualizer event handle initiate.
--
-- The widget holds one entire X event loop,
-- so related resources will be disposed of on kill.
deskVisInitiate :: XIO DeskVisRcvs
deskVisInitiate = do
  rootWin <- xWindow

  desktopStats <- errorAct $ watchXQuery rootWin getDesktopStat pure
  windowsList <- errorAct $ watchXQuery rootWin getAllWindows pure
  windowActive <- errorAct $ watchXQuery rootWin getActiveWindow pure

  -- MAYBE log warning when window without the info is detected?
  trackWinInfo <- xQueryOnce $ \window -> fmap eitherMay . runExceptT $ do
    winDesktop <- ExceptT $ watchXQuery window getWindowDesktop pure
    winInfo <- ExceptT $ watchXQuery window getWindowInfo pure
    pure PerWinRcvs{..}

  reqActivate <- reqActiveWindow True
  reqToDesktop <- reqCurrentDesktop

  winGetIcon <- xQueryOnce $ \window -> do
    first (formatXQError window) <$> xOnWindow window (runXQuery getWindowIcon)

  pure DeskVisRcvs{..}
  where
    eitherMay = either (const Nothing) Just
    onError window err = do
      liftIO (fail $ formatXQError window err)
    errorAct act = do
      window <- xWindow
      act >>= either (onError window) pure
