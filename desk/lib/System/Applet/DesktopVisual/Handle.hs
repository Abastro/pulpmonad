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
import Data.IntMap.Strict qualified as IM
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

-- FIXME Consider that in XMonad, "Workspace" has String ID.
-- In fact, index notion of XMonad is not static (consult ewmh and its sort) - Must be fixed!

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

  let updateDesk = updateDesktop deskSetup (Glib.new View.DesktopItemView [])
      updateWin = updateWindow (Glib.new View.WindowItemView []) trackWinInfo

  compile $ do
    eDesktopList <- liftIO (taskToSource desktopStats) >>= sourceEvent
    eWindowList <- liftIO (taskToSource windowsList) >>= sourceEvent
    eActiveWindow <- liftIO (taskToSource windowActive) >>= sourceEvent

    (eDesktops, bDesktops) <- desktopList updateDesk eDesktopList
    (eWindows, bWindows) <- windowMap updateWin eWindowList
    bActiveWindow <- stepper Nothing eActiveWindow
    let bDeskViews = V.map desktopView <$> bDesktops
        bWinViews = M.map windowView <$> bWindows

    -- Behavior updates at next step of eWindows, in sync with bWinView
    winDeskPairs <- switchB (pure S.empty) (asWinDeskPairs <$> eWindows)
    let winDeskPairMap = asViewPairMap <$> bDeskViews <*> bWinViews <*> winDeskPairs

    -- Compute and apply container differences.
    deskDiffs <- updateEvent computeDiffs bDeskViews bDeskViews
    pairDiffs <- updateEvent computeDiffs winDeskPairMap winDeskPairMap
    reactimate' $ fmap @Future (Gtk.uiSingleRun . applyDeskDiff mainView) <$> deskDiffs
    reactimate' $ fmap @Future (Gtk.uiSingleRun . applyPairDiff) <$> pairDiffs

    -- Window list update changes window order.
    reactimate $ Gtk.uiSingleRun <$> (applyWindowPriority <$> bDesktops <@> eWindows)

    -- Visibility depends on both desktop state and window count.
    let deskWithWinCnt = pairDeskCnt <$> winDeskPairs <*> bDesktops
    syncBehavior deskWithWinCnt (Gtk.uiSingleRun . traverse_ (uncurry $ applyDesktopVisible deskSetup))

    eDesktopClicks <- switchE never (desktopClicks <$> eDesktops)
    reactimate $ Gtk.uiSingleRun . traverse_ reqToDesktop <$> eDesktopClicks
    pure ()

  Gtk.toWidget mainView
  where
    asWinDeskPairs :: M.Map Window WinItem -> Behavior (S.Set (Window, Int))
    asWinDeskPairs = fmap mapToPairs . traverse bWindowDesktop

    -- Pair with desktop counts
    pairDeskCnt :: S.Set (Window, Int) -> V.Vector a -> V.Vector (NumWindows, a)
    pairDeskCnt pairMap = V.imap $ \i v -> (fromMaybe 0 (deskHisto IM.!? i), v)
      where
        pairToCnt (_win, desk) = (desk, 1 :: NumWindows)
        deskHisto = IM.fromListWith (+) . map pairToCnt $ S.toList pairMap

data DeskItem = DeskItem
  { desktopView :: !View.DesktopItemView
  , desktopStat :: !DesktopStat
  , eDesktopClick :: !(Event ())
  }

data WinItem = WinItem
  { windowView :: !View.WindowItemView
  , winIndex :: !Int
  -- ^ Index in the window list.
  , bWindowDesktop :: !(Behavior Int)
  , eWindowClick :: !(Event ())
  }

mapToPairs :: (Ord a, Ord b) => M.Map a b -> S.Set (a, b)
mapToPairs = S.fromList . M.toList

type ViewPair = (View.WindowItemView, View.DesktopItemView)

asViewPairMap ::
  V.Vector View.DesktopItemView ->
  M.Map Window View.WindowItemView ->
  S.Set (Window, Int) ->
  M.Map (Window, Int) ViewPair
asViewPairMap desktops windows = M.mapMaybe pairToView . M.fromSet id
  where
    pairToView (win, desk) = (windows M.! win,) <$> (desktops V.!? desk)

-- Uses list since not so many events occur simultaneously
desktopClicks :: V.Vector DeskItem -> Event [Int]
desktopClicks desktops = fold (V.imap eWithIdx desktops)
  where
    eWithIdx idx DeskItem{eDesktopClick} = [idx] <$ eDesktopClick

-- Problem: Looks generalizable, while presence IO action complicates the matter.
-- Remove first and Add in order, this part might need to be considered.
applyDeskDiff :: View.DesktopVisual -> Diffs (V.Vector View.DesktopItemView) -> IO ()
applyDeskDiff main Diffs{..} = do
  -- Remove first, then add "in order".
  traverse_ (View.removeDesktop main) removed
  traverse_ (View.addDesktop main) added

-- MAYBE Log these occurrences
applyPairDiff :: Diffs (M.Map k ViewPair) -> IO ()
applyPairDiff Diffs{..} = do
  traverse_ (uncurry . flip $ View.removeWindow) removed
  traverse_ (uncurry . flip $ View.insertWindow) added

-- TODO Need a test that same ID points towards the same View. Likely require restructure.
desktopList ::
  (Maybe DeskItem -> DesktopStat -> MomentIO DeskItem) ->
  Event (V.Vector DesktopStat) ->
  MomentIO (Discrete (V.Vector DeskItem))
desktopList update eStat = exeAccumD V.empty (flip (zipToRightM update) <$> eStat)

updateDesktop ::
  DesktopSetup ->
  IO View.DesktopItemView ->
  Maybe DeskItem ->
  DesktopStat ->
  MomentIO DeskItem
updateDesktop setup mkView old desktopStat = do
  desktop <- maybe createDesktop (pure . updated) old
  liftIO $ applyDesktopState setup desktop
  pure desktop
  where
    createDesktop = do
      desktopView <- liftIO mkView
      eDesktopClick <- sourceEvent (View.desktopClickSource desktopView)
      pure DeskItem{..}
    updated desktop = desktop{desktopStat}

windowMap ::
  (Maybe WinItem -> (Window, Int) -> MomentIO (Maybe WinItem)) ->
  Event (V.Vector Window) ->
  MomentIO (Discrete (M.Map Window WinItem))
windowMap update eList = do
  let eWinIdx = asMapWithIdx <$> eList
  exeAccumD M.empty (flip (filterZipToRightM update) <$> eWinIdx)
  where
    asMapWithIdx list = M.mapWithKey (,) . M.fromList $ zip (V.toList list) [0 ..]

updateWindow ::
  IO View.WindowItemView ->
  (Window -> IO (Maybe PerWinRcvs)) ->
  Maybe WinItem ->
  (Window, Int) ->
  MomentIO (Maybe WinItem)
updateWindow mkView trackInfo old (windowId, winIndex) = runMaybeT $ updated <|> createWindow
  where
    createWindow = do
      PerWinRcvs{..} <- MaybeT . liftIO $ trackInfo windowId
      windowView <- liftIO mkView
      lift $ do
        bWindowDesktop <- taskToBehavior winDesktop
        eWinInfo <- liftIO (taskToSource winInfo) >>= sourceEvent
        eWindowClick <- sourceEvent (View.windowClickSource windowView)

        -- Apply the received information on the window
        reactimate (applyWindowState windowView <$> eWinInfo)
        -- TODO Apply Icon - how?

        pure WinItem{..}
    updated = MaybeT $ pure (setIndex <$> old)
    setIndex window = window{winIndex}

-- | Update priority of windows, then reflect the priority.
applyWindowPriority :: V.Vector DeskItem -> M.Map k WinItem -> IO ()
applyWindowPriority desktops windows = do
  -- MAYBE Too complex? Perhaps better to embed inside window
  -- Perhaps, all the widgets can be read from single file
  for_ windows $ \WinItem{..} -> View.setPriority windowView winIndex
  for_ desktops $ \DeskItem{desktopView} -> View.reflectPriority desktopView

applyDesktopState :: DesktopSetup -> DeskItem -> IO ()
applyDesktopState DesktopSetup{..} DeskItem{desktopStat = DesktopStat{..}, ..} = do
  View.desktopSetLabel desktopView (desktopLabeling desktopName)
  View.desktopSetState desktopView (viewState desktopState)
  where
    viewState = \case
      DeskActive -> View.DesktopActive
      DeskVisible -> View.DesktopVisible
      DeskHidden -> View.DesktopHidden

applyDesktopVisible :: DesktopSetup -> NumWindows -> DeskItem -> IO ()
applyDesktopVisible DesktopSetup{..} numWin DeskItem{..} = do
  View.desktopSetVisible desktopView (showDesktop desktopStat numWin)

applyWindowState :: View.WindowItemView -> WindowInfo -> IO ()
applyWindowState view WindowInfo{..} = do
  Gtk.widgetSetTooltipText view (Just windowTitle)
  View.windowSetStates view (map viewState $ S.toList windowState)
  where
    viewState = \case
      WinHidden -> View.WindowHidden
      WinDemandAttention -> View.WindowDemanding

data SpecifyIcon
  = AppIconFrom !(V.Vector T.Text)
  | CustomFrom !WindowInfo
  | NoIcon

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
  -- Icons could be watched, which adds another layer..
  -- Perhaps: Only classes change invoke different specifier.
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
Window click: reqActivate window
Desktop click: reqToDesktop desktop

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

-}

{-------------------------------------------------------------------
                          Application Info
--------------------------------------------------------------------}

-- MAYBE Perhaps consulting for process id is better
-- libwnck application.c and window.c might have relevant logic
--
-- ..apparently it uses _NET_WM_ICON as a better choice. (Even uses WM_HINTS)
--
-- PID does not provide info of corresponding desktop file, so the part does need heuristics.
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
