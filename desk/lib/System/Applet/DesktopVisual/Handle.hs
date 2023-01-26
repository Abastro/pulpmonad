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
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Maybe
import Data.Bifunctor (first)
import Data.Foldable
import Data.IORef
import Data.Map.Merge.Strict qualified as M
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time.Clock.POSIX
import Data.Traversable
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
  }

deskVisualizer ::
  (MonadUnliftIO m, MonadLog m, MonadXHand m, MonadPulpPath m) =>
  DesktopSetup ->
  WindowSetup ->
  m Gtk.Widget
deskVisualizer deskSetup winSetup = withRunInIO $ \unlift -> do
  DeskVisRcvs{..} <- unlift $ runXHand deskVisInitiate
  mainView <- View.mainView

  compile $ do
    desktopUpdate <- liftIO (taskToSource desktopStats) >>= sourceEvent
    windowListUpdate <- liftIO (taskToSource windowsList) >>= sourceEvent
    activeWindowUpdate <- liftIO (taskToSource windowActive) >>= sourceEvent

    desktops <- desktopList (unlift View.deskItemView) desktopUpdate
    windows <- windowMap (unlift View.winItemView) trackWinInfo windowListUpdate
    activeWindow <- stepper Nothing activeWindowUpdate
    let deskViews = V.map desktopView <$> desktops
        -- In fact, we need to track pairs (window, desktop)
        deskWindows = flipWinDesk <$> windows
        deskWindowCnt = M.map (fromIntegral . V.length) <$> deskWindows
        deskWithWinCnt = withCount <$> deskWindowCnt <*> desktops

    syncBehaviorDiff deskViews (fmap Gtk.uiSingleRun . mainSyncDesktop mainView)
    syncBehavior deskWithWinCnt (Gtk.uiSingleRun . traverse_ (uncurry $ reflectDesktop deskSetup))

    deskChanges <- differences windows windowDeskDiff
    let deskChangesIdxed = fmap fmap (winDeskIdxed <$> deskViews) <@> deskChanges

    undefined
  undefined
  where
    -- Window -> Desk to Desk -> Win
    -- Vector is needed to track the index!
    -- ..this is complex.
    flipWinDesk = M.map (V.fromList . map snd . sortOn fst) . M.fromListWith (<>) . map toIdxPair . M.toList
      where
        toIdxPair (win, WinItemHandle{..}) = (winItemDesktop, [(winIndex, win)])

    withCount cnts = V.imap $ \i v -> (fromMaybe 0 (cnts M.!? i), v)

    -- Missing is interpreted as -1.
    windowDeskDiff = M.merge @Window onRemove onAdd onChange
      where
        onRemove = M.mapMissing $ \_ WinItemHandle{..} -> (windowView, winItemDesktop, -1)
        onAdd = M.mapMissing $ \_ WinItemHandle{..} -> (windowView, -1, winItemDesktop)
        onChange = M.zipWithMatched $ \_ old new -> (windowView new, winItemDesktop old, winItemDesktop new)
    winDeskIdxed desks = M.map $ \(view, old, new) -> (view, desks V.!? old, desks V.!? new)

    winChangeDesktop _ (winView, oldDesk, newDesk) = do
      -- TODO Find index of window among desktop!
      undefined

data DeskItemHandle = DeskItemHandle
  { desktopView :: !View.DeskItemView
  , deskItemStat :: !DesktopStat
  }

-- How would I handle click event?
desktopList ::
  IO View.DeskItemView ->
  Event (V.Vector DesktopStat) ->
  MomentIO (Behavior (V.Vector DeskItemHandle))
desktopList mkView updates = do
  -- Starts empty to add later
  rec list <- stepper V.empty newList
      let views = V.map desktopView <$> list
          mkNew = getNew <$> views <@> updates
      newList <- mapEventIO id mkNew
  pure list
  where
    -- TODO Remove IO involved here, somehow - avoid creating views?
    -- Maybe simulate "lazy" with IO? Still hard.

    -- new matches update exactly
    getNew old = V.imapM $ \i stat -> (`DeskItemHandle` stat) <$> maybe mkView pure (old V.!? i)

-- Can I phase this out? Create action could take care of it, after all.
mainSyncDesktop :: View.MainView -> V.Vector View.DeskItemView -> V.Vector View.DeskItemView -> IO ()
mainSyncDesktop View.MainView{..} old new = do
  -- Remove first, then add "in order".
  traverse_ mainRemoveDesktop $ V.drop (V.length new) old
  traverse_ mainAddDesktop $ V.drop (V.length old) new

reflectDesktop :: DesktopSetup -> NumWindows -> DeskItemHandle -> IO ()
reflectDesktop DesktopSetup{..} numWin (DeskItemHandle View.DeskItemView{..} stat@DesktopStat{..}) = do
  deskSetName (desktopLabeling desktopName)
  deskSetVisible (showDesktop stat numWin)
  deskSetState desktopState

data WinItemHandle = WinItemHandle
  { windowView :: !View.WinItemView
  , winIndex :: !Int
  -- ^ Index in the window list.
  , winItemDesktop :: !Int
  , winItemInfo :: !WindowInfo
  }

windowMap ::
  IO View.WinItemView ->
  (Window -> IO (Maybe PerWinRcvs)) ->
  Event (V.Vector Window) ->
  MomentIO (Behavior (M.Map Window WinItemHandle))
windowMap mkView trackInfo updates = do
  -- Map of behaviors
  rec mapB <- stepper M.empty newMapB
      let mkNewUpd = getNewWithUpdate <$> mapB <@> updates
      newMapUpd <- execute mkNewUpd
      let newMapB = fst <$> newMapUpd
  -- Updates whenever the handle changes
  switchB (pure M.empty) $ sequenceA <$> newMapB
  where
    getNewWithUpdate old update = (,update) <$> getNew old update
    getNew old update = M.mergeA onRemove onAdd onPersist old (vecToMap update)
    vecToMap = M.fromList . (`zip` [0 ..]) . V.toList

    -- Destroy action is not performed here
    onRemove = M.dropMissing
    -- Track window here to drop the window without info
    onAdd = M.traverseMaybeMissing $ \window idx ->
      liftIO (trackInfo window) >>= \case
        Nothing -> pure Nothing
        Just PerWinRcvs{..} -> do
          view <- liftIO mkView
          -- Both properties are available from the get go.
          bDesktop <- taskToBehavior winDesktop
          bInfo <- taskToBehavior winInfo
          pure . Just $ WinItemHandle view idx <$> bDesktop <*> bInfo
    onPersist = M.zipWithMatched $ \_ behav newIdx -> (\handle -> handle{winIndex = newIdx}) <$> behav

{-
-- | Desktops visualizer widget. Forks its own X11 event handler.
deskVisualizer ::
  (MonadUnliftIO m, MonadLog m, MonadXHand m) =>
  DesktopSetup ->
  WindowSetup ->
  m Gtk.Widget
deskVisualizer deskSetup winSetup = do
  rcvs <- runXHand deskVisInitiate
  deskVisualView <- Glib.new View.DesktopVisual []
  DeskVisHandle <- deskVisMake rcvs (deskSetup, winSetup) deskVisualView

  Gtk.toWidget deskVisualView

-- Currently handle is empty, because no external handling is permitted.
data DeskVisHandle = DeskVisHandle

-- TODO Model scheme is not working, need to make it work with pure states.

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

appInfoImgSetter :: (MonadUnliftIO m, MonadLog m) => AppInfoCol -> WindowInfo -> MaybeT m Gio.Icon
appInfoImgSetter appCol WindowInfo{windowClasses} = do
  allDat <- liftIO $ getAppInfos appCol
  let findWith matcher = V.find (\dat -> any (matcher dat) windowClasses) allDat
  appDat@AppInfoData{appId} <- MaybeT . pure $ findWith classMatch <|> findWith identMatch <|> findWith execMatch
  lift $ logS (T.pack "DeskVis") LevelDebug $ logStrf "AppInfo: $1 -> $2" (show windowClasses) appId
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
