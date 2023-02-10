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
  -- Due to implementation concerns, only class change would allow custom to start acting for icon set.
  }

deskVisualizer ::
  (MonadUnliftIO m, MonadLog m, MonadXHand m) =>
  DesktopSetup ->
  WindowSetup ->
  m Gtk.Widget
deskVisualizer deskSetup winSetup = withRunInIO $ \unlift -> do
  DeskVisRcvs{..} <- unlift $ runXHand deskVisInitiate
  appInfoCol <- trackAppInfo
  mainView <- Glib.new View.DesktopVisual []

  let getRawIcon = unlift . wrapGetRaw . winGetIcon
      specifyIcon = fmap unlift . specifyWindowIcon winSetup appInfoCol
      updateDesk = updateDesktop deskSetup (Glib.new View.DesktopItemView [])
      updateWin = updateWindow (Glib.new View.WindowItemView []) trackWinInfo getRawIcon specifyIcon

  compile $ do
    eDesktopList <- sourceEvent (taskToSource desktopStats)
    eWindowList <- sourceEvent (taskToSource windowsList)
    eActiveWindow <- sourceEvent (taskToSource windowActive)

    (eDesktops, bDesktops) <- desktopList updateDesk eDesktopList
    (eWindows, bWindows) <- windowMap updateWin eWindowList
    bActiveWindow <- stepper Nothing eActiveWindow
    let bDeskViews = V.map desktopView <$> bDesktops
        bWinViews = M.map windowView <$> bWindows
        -- Can change when window list changes, for window list change could lag behind.
        -- The Maybe-bind is intended.
        bActiveView = (\views win -> (views M.!?) =<< win) <$> bWinViews <*> bActiveWindow

    -- Behavior updates at next step of eWindows, in sync with bWinView
    winDeskPairs <- switchB (pure S.empty) (asWinDeskPairs <$> eWindows)
    let bWinDeskPairMap = asViewPairMap <$> bDeskViews <*> bWinViews <*> winDeskPairs

    -- Compute and apply container differences.
    eDeskDiffs <- diffEvent (<--) (AsCacheStack <$> bDeskViews)
    ePairDiffs <- diffEvent (<--) bWinDeskPairMap
    reactimate' $ fmap @Future (Gtk.uiSingleRun . applyDeskDiff mainView) <$> eDeskDiffs
    reactimate' $ fmap @Future (Gtk.uiSingleRun . applyPairDiff) <$> ePairDiffs

    -- Sync with activated window.
    activeDiffs <- diffEvent (<--) bActiveView
    reactimate' $ fmap @Future (Gtk.uiSingleRun . applyActiveWindow) <$> activeDiffs

    -- Window list update changes window order. We do not care of desktop changes.
    reactimate $ Gtk.uiSingleRun <$> (applyWindowPriority <$> bDesktops <@> eWindows)

    -- Visibility depends on both desktop state and window count.
    let deskWithWinCnt = pairDeskCnt <$> winDeskPairs <*> bDesktops
    syncBehavior deskWithWinCnt (Gtk.uiSingleRun . traverse_ (uncurry $ applyDesktopVisible deskSetup))

    -- Activations to send.
    eDesktopActivate <- switchE never (desktopActivates <$> eDesktops)
    eWindowActivate <- switchE never (windowActivates <$> eWindows)
    reactimate $ Gtk.uiSingleRun . traverse_ reqToDesktop <$> eDesktopActivate
    reactimate $ Gtk.uiSingleRun . traverse_ reqActivate <$> eWindowActivate

  Gtk.toWidget mainView
  where
    asWinDeskPairs :: M.Map Window WinItem -> Behavior (S.Set (Window, Int))
    asWinDeskPairs = fmap (S.fromList . M.toList) . traverse bWindowDesktop

    -- Pair with desktop counts.
    pairDeskCnt :: S.Set (Window, Int) -> V.Vector a -> V.Vector (NumWindows, a)
    pairDeskCnt pairMap = V.imap $ \i v -> (fromMaybe 0 (deskHisto IM.!? i), v)
      where
        pairToCnt (_win, desk) = (desk, 1 :: NumWindows)
        deskHisto = IM.fromListWith (+) . map pairToCnt $ S.toList pairMap

data DeskItem = DeskItem
  { desktopView :: !View.DesktopItemView
  , desktopStat :: !DesktopStat
  , eDesktopClick :: !(Event ())
  , deskDelete :: !(IO ())
  }

data WinItem = WinItem
  { windowView :: !View.WindowItemView
  , winIndex :: !Int
  -- ^ Index in the window list.
  , bWindowDesktop :: !(Behavior Int)
  , eWindowClick :: !(Event ())
  , winDelete :: !(IO ())
  }

type ViewPair = (View.WindowItemView, View.DesktopItemView)

asViewPairMap ::
  V.Vector View.DesktopItemView ->
  M.Map Window View.WindowItemView ->
  S.Set (Window, Int) ->
  CacheMap (Window, Int) ViewPair
asViewPairMap desktops windows = AsCacheMap . M.mapMaybe pairToView . M.fromSet id
  where
    pairToView (win, desk) = (windows M.! win,) <$> (desktops V.!? desk)

-- Uses list since not so many events occur simultaneously
desktopActivates :: V.Vector DeskItem -> Event [Int]
desktopActivates = fold . V.imap eWithIdx
  where
    eWithIdx idx DeskItem{eDesktopClick} = [idx] <$ eDesktopClick

windowActivates :: M.Map Window WinItem -> Event [Window]
windowActivates = M.foldMapWithKey $ \win WinItem{eWindowClick} -> [win] <$ eWindowClick

applyDeskDiff :: View.DesktopVisual -> PatchOf (ColOp View.DesktopItemView) -> IO ()
applyDeskDiff main = applyImpure $ \case
  Insert desktop -> View.insertDesktop main desktop
  Delete desktop -> View.removeDesktop main desktop

applyPairDiff :: PatchOf (ColOp ViewPair) -> IO ()
applyPairDiff = applyImpure $ \case
  Insert (window, desktop) -> View.insertWindow desktop window
  Delete (window, desktop) -> View.removeWindow desktop window

-- TODO Need a test that same ID points towards the same View. Likely require restructure.
desktopList ::
  (Maybe DeskItem -> DesktopStat -> MomentIO DeskItem) ->
  Event (V.Vector DesktopStat) ->
  MomentIO (Discrete (V.Vector DeskItem))
desktopList update eStat = exeAccumD V.empty (updateFn <$> eStat)
  where
    updateFn stats olds = do
      news <- zipToRightM update olds stats
      news <$ onRemainderRight (liftIO . deskDelete) news olds

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
      (eDesktopClick, free1) <- sourceEventWA (View.desktopClickSource desktopView)
      let deskDelete = free1
      pure DeskItem{..}
    updated desktop = desktop{desktopStat}

windowMap ::
  (Maybe WinItem -> (Window, Int) -> MomentIO (Maybe WinItem)) ->
  Event (V.Vector Window) ->
  MomentIO (Discrete (M.Map Window WinItem))
windowMap update eList = do
  let eWinIdx = asMapWithIdx <$> eList
  exeAccumD M.empty (updateFn <$> eWinIdx)
  where
    asMapWithIdx list = M.mapWithKey (,) . M.fromList $ zip (V.toList list) [0 ..]
    updateFn winIdxs olds = do
      news <- M.traverseMaybeWithKey (const id) $ zipToRight update olds winIdxs
      news <$ onRemainderRight (liftIO . winDelete) news olds

updateWindow ::
  IO View.WindowItemView ->
  (Window -> IO (Maybe PerWinRcvs)) ->
  (Window -> IO [Gtk.RawIcon]) ->
  ((Bool, WindowInfo) -> (Bool, IconSpecify) -> IO (Bool, IconSpecify)) ->
  Maybe WinItem ->
  (Window, Int) ->
  MomentIO (Maybe WinItem)
updateWindow mkView trackInfo winGetRaw updateSpecify old (windowId, winIndex) = runMaybeT $ updated <|> createWindow
  where
    createWindow = do
      PerWinRcvs{..} <- MaybeT . liftIO $ trackInfo windowId
      windowView <- liftIO mkView
      lift $ do
        (bWindowDesktop, free1) <- taskToBehaviorWA winDesktop
        (eWinInfo, free2) <- sourceEventWA (taskToSource winInfo)
        (eWindowClick, free3) <- sourceEventWA (View.windowClickSource windowView)

        -- Specify the window icon
        eClassChangeWithInfo <- accumE (False, error "no init") (addClassChange <$> eWinInfo)
        (eSpecify, _) <- exeAccumD (False, FromEWMH) (fmap liftIO . updateSpecify <$> eClassChangeWithInfo)
        -- MAYBE Filtering is not ideal
        let eSpecifyCh = snd <$> filterE fst eSpecify

        -- Apply the received information on the window
        free4 <- reactEvent (Gtk.uiSingleRun . applyWindowState windowView <$> eWinInfo)
        free5 <- reactEvent (Gtk.uiSingleRun . applySpecifiedIcon (winGetRaw windowId) windowView <$> eSpecifyCh)
        let winDelete = free1 <> free2 <> free3 <> free4 <> free5

        pure WinItem{..}
    updated = MaybeT $ pure (setIndex <$> old)
    setIndex window = window{winIndex}

    addClassChange newInfo (_, oldInfo) = (windowClasses newInfo /= windowClasses oldInfo, oldInfo)

-- | Update priority of windows, then reflect the priority.
applyWindowPriority :: V.Vector DeskItem -> M.Map k WinItem -> IO ()
applyWindowPriority desktops windows = do
  -- MAYBE Too complex? Perhaps better to embed inside window
  -- Perhaps, all the widgets can be read from single file
  for_ windows $ \WinItem{..} -> View.setPriority windowView winIndex
  for_ desktops $ \DeskItem{desktopView} -> View.reflectPriority desktopView

-- Patch only used to represent both old & new
applyActiveWindow :: PatchOf (ColOp View.WindowItemView) -> IO ()
applyActiveWindow = applyImpure $ \case
  Insert window -> View.windowSetActivate window True
  Delete window -> View.windowSetActivate window False

applyDesktopState :: DesktopSetup -> DeskItem -> IO ()
applyDesktopState DesktopSetup{..} DeskItem{desktopStat = DesktopStat{..}, ..} = do
  View.desktopSetLabel desktopView (desktopLabeling desktopName)
  View.desktopSetState desktopView desktopState

applyDesktopVisible :: DesktopSetup -> NumWindows -> DeskItem -> IO ()
applyDesktopVisible DesktopSetup{..} numWin DeskItem{..} = do
  View.desktopSetVisible desktopView (showDesktop desktopStat numWin)

applyWindowState :: View.WindowItemView -> WindowInfo -> IO ()
applyWindowState view WindowInfo{..} = do
  Gtk.widgetSetTooltipText view (Just windowTitle)
  View.windowSetStates view (S.toList windowState)

-- From EWMH is the default
data IconSpecify = FromAppIcon !Gio.Icon | FromCustom !Gio.Icon | FromEWMH

specifyWindowIcon ::
  (MonadUnliftIO m, MonadLog m) =>
  WindowSetup ->
  AppInfoCol ->
  (Bool, WindowInfo) ->
  (Bool, IconSpecify) ->
  m (Bool, IconSpecify)
specifyWindowIcon WindowSetup{..} appCol (classChanged, winInfo) (_, oldSpecify) = withRunInIO $ \unlift -> do
  fromMaybe (wasEWMH, FromEWMH) <$> runMaybeT (mapMaybeT unlift setWithApp <|> setWithCustom)
  where
    WindowInfo{windowClasses} = winInfo
    -- Class change: All re-evaluates from front to back.
    -- Class no change: Only responds to the corresponding ones.
    setWithApp = case (oldSpecify, classChanged) of
      (_, True) -> (True,) . FromAppIcon <$> appInfoImgSetter appCol windowClasses
      -- Should not change when class did not change.
      (FromAppIcon _, False) -> pure (False, oldSpecify)
      (_, False) -> empty
    setWithCustom = case (oldSpecify, classChanged) of
      (_, True) -> (True,) . FromCustom <$> windowImgIcon winInfo
      -- Actively updates to the window info changes.
      (FromCustom _, False) -> (True,) . FromCustom <$> windowImgIcon winInfo
      (_, False) -> empty
    wasEWMH = case oldSpecify of
      -- Never updates. Currently do not support animation by EWMH.
      FromEWMH -> False
      _ -> True

applySpecifiedIcon ::
  IO [Gtk.RawIcon] ->
  View.WindowItemView ->
  IconSpecify ->
  IO ()
applySpecifiedIcon getRaw view = \case
  FromAppIcon icon -> View.windowSetGIcon view icon
  FromCustom icon -> View.windowSetGIcon view icon
  FromEWMH -> View.windowSetRawIcons view =<< getRaw

-- Wraps the raw icon getter so that exception is cared for.
wrapGetRaw :: (MonadUnliftIO m, MonadLog m) => GetXIcon -> m [Gtk.RawIcon]
wrapGetRaw getXIcon =
  liftIO getXIcon >>= \case
    Left err -> [] <$ logS (T.pack "DeskVis") LevelDebug (logStrf "Cannot recognize icon due to: $1" err)
    Right icons -> pure icons

{-
  TODO Consider: Log desktop changes?
  unlift $ logS (T.pack "DeskVis") LevelDebug $ logStrf "[$1] desktop $2 -> $3" windowId deskOld deskNew
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
