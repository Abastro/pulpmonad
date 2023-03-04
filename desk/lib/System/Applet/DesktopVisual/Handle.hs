{-# LANGUAGE RecursiveDo #-}

module System.Applet.DesktopVisual.Handle (
  NumWindows,
  GetXIcon,
  DesktopSetup (..),
  WindowSetup (..),
  deskVisualizer,
) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Event.Entry
import Control.Event.State
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Maybe
import Data.Bifunctor (Bifunctor (..), first)
import Data.Foldable
import Data.GI.Base.Constructible qualified as Glib
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Monoid
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
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
import System.Applet.DesktopVisual.DesktopItemView qualified as DeskView
import System.Applet.DesktopVisual.DesktopVisual qualified as MainView
import System.Applet.DesktopVisual.WindowItemView qualified as WinView
import System.Log.LogPrint
import System.Pulp.PulpEnv
import Control.Concurrent

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
  { windowImgIcon :: V.Vector T.Text -> Maybe (WindowInfo -> IO Gio.Icon)
  -- ^ With which icon the window is going to set to.
  -- Whether to go with this one or not is only dependent on the window class.
  }

logName = T.pack "DeskVis"

deskVisualizer :: DesktopSetup -> WindowSetup -> PulpIO Gtk.Widget
deskVisualizer deskSetup winSetup = withRunInIO $ \unlift -> do
  DeskVisRcvs{..} <- unlift $ runXHook deskVisInitiate
  appInfoCol <- trackAppInfo
  -- UI thread here
  mainView <- Glib.new MainView.AsView []

  let updateDesk = updateDesktop deskSetup (Gtk.uiCreate $ Glib.new DeskView.AsView [])
      updateWin = updateWindow unlift winSetup appInfoCol (Gtk.uiCreate $ Glib.new WinView.AsView []) trackWinInfo

  actuated <- newEmptyMVar
  -- TODO Require forkIO on network setup
  -- TOOD Initialization event could be better approach than "inited" everywhere
  forkIO $ do
    network <- compile $ do
      iDesktopList <- stepsInited desktopStats
      iWindowList <- stepsInited windowsList
      bActiveWindow <- stepsBehavior windowActive

      (iDesktops, bDesktops) <- desktopList updateDesk iDesktopList
      (iWindows, bWindows) <- windowMap updateWin iWindowList
      let bDeskViews = V.map (\desk -> desk.view) <$> bDesktops
          bWinViews = M.map (\win -> win.view) <$> bWindows
          -- Can change when window list changes, for window list change could lag behind.
          -- The Maybe-bind is intended, for the view might not exist yet.
          bActiveView = (\views win -> (views M.!?) =<< win) <$> bWinViews <*> bActiveWindow

      -- Behavior updates at next step of eWindows, in sync with bWinView
      bWinDeskPairs <- switchIB (asWinDeskPairs <$> iWindows)
      let bWinDeskPairMap = asViewPairMap <$> bDeskViews <*> bWinViews <*> bWinDeskPairs

      -- Compute and apply container differences.
      syncBehaviorDiff (-->) mempty (AsCacheStack <$> bDeskViews) (applyDeskDiff mainView)
      syncBehaviorDiff (-->) mempty bWinDeskPairMap applyPairDiff

      -- Sync with activated window.
      syncBehaviorDiff (-->) Nothing bActiveView applyActiveWindow

      -- Window list update changes window order in batch. We do not care of desktop changes.
      liftIOLater $ applyWindowPriority iDesktops.initial iWindows.initial
      reactimate $ applyWindowPriority <$> bDesktops <@> iWindows.update

      -- Visibility depends on both desktop state and window count.
      let deskWithWinCnt = pairDeskCnt <$> bWinDeskPairs <*> bDesktops
      syncBehavior deskWithWinCnt $ traverse_ (uncurry $ applyDesktopVisible deskSetup)

      -- Activations to send to X11.
      eDesktopActivate <- switchIE (desktopActivates <$> iDesktops)
      eWindowActivate <- switchIE (windowActivates <$> iWindows)

      reactimate $ traverse_ reqToDesktop <$> eDesktopActivate
      reactimate $ traverse_ reqActivate <$> eWindowActivate
    actuate network
  putMVar actuated ()

  Gtk.toWidget mainView
  where
    asWinDeskPairs :: M.Map Window WinItem -> Behavior (S.Set (Window, Int))
    asWinDeskPairs = fmap (S.fromList . M.toList) . traverse (\desk -> desk.bWindowDesktop)

    -- Pair with desktop counts.
    pairDeskCnt :: S.Set (Window, Int) -> V.Vector a -> V.Vector (NumWindows, a)
    pairDeskCnt pairMap = V.imap $ \i v -> (fromMaybe 0 (deskHisto IM.!? i), v)
      where
        pairToCnt (_win, desk) = (desk, 1 :: NumWindows)
        deskHisto = IM.fromListWith (+) . map pairToCnt $ S.toList pairMap

data DeskItem = DeskItem
  { view :: !DeskView.View
  , desktopStat :: !DesktopStat
  , eDesktopClick :: !(Event ())
  , delete :: !(IO ())
  }

data WinItem = WinItem
  { view :: !WinView.View
  , winIndex :: !Int
  -- ^ Index in the window list.
  , bWindowDesktop :: !(Behavior Int)
  , eWindowClick :: !(Event ())
  , delete :: !(IO ())
  }

type ViewPair = (WinView.View, DeskView.View)

asViewPairMap ::
  V.Vector DeskView.View ->
  M.Map Window WinView.View ->
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

applyDeskDiff :: MainView.View -> PatchOf (ColOp DeskView.View) -> IO ()
applyDeskDiff main = applyImpure $ \case
  Insert desktop -> MainView.insertDesktop main desktop
  Delete desktop -> MainView.removeDesktop main desktop

applyPairDiff :: PatchOf (ColOp ViewPair) -> IO ()
applyPairDiff = applyImpure $ \case
  Insert (window, desktop) -> DeskView.insertWindow desktop window
  Delete (window, desktop) -> DeskView.removeWindow desktop window

-- TODO Need a test that same ID points towards the same View. Likely require restructure.
desktopList ::
  (Maybe DeskItem -> DesktopStat -> MomentIO DeskItem) ->
  InitedEvent (V.Vector DesktopStat) ->
  MomentIO (InitedEvent (V.Vector DeskItem), Behavior (V.Vector DeskItem))
desktopList update iStat = do
  let iUpdates = updateFn <$> iStat
  initList <- iUpdates.initial V.empty
  (eList, bList) <- exeAccumD initList iUpdates.update
  pure (MkInited initList eList, bList)
  where
    updateFn stats olds = do
      news <- zipToRightM update olds stats
      news <$ onRemainderRight (\desk -> liftIO desk.delete) news olds

updateDesktop ::
  DesktopSetup ->
  IO (MVar DeskView.View) ->
  Maybe DeskItem ->
  DesktopStat ->
  MomentIO DeskItem
updateDesktop setup mkView old desktopStat = do
  desktop <- maybe createDesktop (pure . updated) old
  liftIO $ applyDesktopState setup desktop
  pure desktop
  where
    createDesktop = do
      view <- liftIO (mkView >>= takeMVar)
      (eDesktopClick, free1) <- sourceEventWA (DeskView.clickSource view)
      let delete = free1
      pure DeskItem{..}
    updated desktop = desktop{desktopStat}

windowMap ::
  (Maybe WinItem -> (Window, Int) -> MomentIO (Maybe WinItem)) ->
  InitedEvent (V.Vector Window) ->
  MomentIO (InitedEvent (M.Map Window WinItem), Behavior (M.Map Window WinItem))
windowMap update iList = do
  let iUpdates = updateFn . asMapWithIdx <$> iList
  initMap <- iUpdates.initial M.empty
  (eMap, bMap) <- exeAccumD initMap iUpdates.update
  pure (MkInited initMap eMap, bMap)
  where
    asMapWithIdx list = M.mapWithKey (,) . M.fromList $ zip (V.toList list) [0 ..]
    updateFn winIdxs olds = do
      news <- M.traverseMaybeWithKey (const id) $ zipToRight update olds winIdxs
      news <$ onRemainderRight (\win -> liftIO win.delete) news olds

updateWindow ::
  (forall a. PulpIO a -> IO a) ->
  WindowSetup ->
  AppInfoCol ->
  IO (MVar WinView.View) ->
  (Window -> IO (Maybe PerWinRcvs)) ->
  Maybe WinItem ->
  (Window, Int) ->
  MomentIO (Maybe WinItem)
updateWindow unlift setup appInfoCol mkView trackInfo old (windowId, winIndex) = runMaybeT $ updated <|> createWindow
  where
    createWindow = do
      PerWinRcvs{..} <- MaybeT . liftIO $ trackInfo windowId
      varWindowView <- liftIO mkView
      lift $ do
        -- Currently, actual listening to window is tied with UI.
        (bWindowDesktop, free1) <- stepsBehaviorWA winDesktop
        (iWinInfo, free2) <- stepsInitedWA winInfo
        bWinInfo <- initedBehavior iWinInfo

        -- Specify the window icon.
        bSpecify <- iconSpecifier unlift setup appInfoCol iWinInfo

        -- Take window view as late as possible.
        view <- liftIO $ takeMVar varWindowView
        (eWindowClick, free3) <- sourceEventWA (WinView.clickSource view)

        -- Apply the received information on the window.
        free4 <- syncBehaviorWA bWinInfo (applyWindowState view)
        free5 <- syncBehaviorWA bSpecify $ applySpecifiedIcon (unlift $ wrapGetRaw getWinIcon) view
        let delete = free1 <> free2 <> free3 <> free4 <> free5

        pure WinItem{..}
    updated = MaybeT $ pure (setIndex <$> old)
    setIndex window = window{winIndex}

-- | Update priority of windows, then reflect the priority.
applyWindowPriority :: V.Vector DeskItem -> M.Map k WinItem -> IO ()
applyWindowPriority desktops windows = do
  -- MAYBE Too complex? Perhaps better to embed inside window

  -- Needs to be run in UI thread.
  for_ windows $ \WinItem{..} -> Gtk.uiSingleRun $ WinView.setPriority view winIndex
  for_ desktops $ \DeskItem{view} -> DeskView.reflectPriority view

-- Patch only used to represent both old & new
applyActiveWindow :: PatchOf (ColOp WinView.View) -> IO ()
applyActiveWindow = applyImpure $ \case
  Insert window -> WinView.setActivate window True
  Delete window -> WinView.setActivate window False

applyDesktopState :: DesktopSetup -> DeskItem -> IO ()
applyDesktopState DesktopSetup{..} DeskItem{desktopStat = DesktopStat{..}, ..} = do
  DeskView.setLabel view (desktopLabeling desktopName)
  DeskView.setState view desktopState

applyDesktopVisible :: DesktopSetup -> NumWindows -> DeskItem -> IO ()
applyDesktopVisible DesktopSetup{..} numWin DeskItem{..} = do
  DeskView.setVisible view (showDesktop desktopStat numWin)

applyWindowState :: WinView.View -> WindowInfo -> IO ()
applyWindowState view WindowInfo{..} = do
  WinView.setTitle view windowTitle
  WinView.setStates view (S.toList windowState)

-- From EWMH is the default
data IconSpecify = FromAppIcon !Gio.Icon | FromCustom !WindowInfo (WindowInfo -> IO Gio.Icon) | FromEWMH

-- | Icon specifier event stream to update the icon.
iconSpecifier ::
  (forall a. PulpIO a -> IO a) ->
  WindowSetup ->
  AppInfoCol ->
  InitedEvent WindowInfo ->
  MomentIO (Behavior IconSpecify)
iconSpecifier unlift setup appInfoCol ieWinInfo = do
  bWinInfo <- initedBehavior ieWinInfo -- Tracks old WinInfo
  let (eClassChange, eInfoChange) = split $ branchClassChange <$> bWinInfo <@> ieWinInfo.update

  rec initSpecify <- onClassChange ieWinInfo.initial
      eSpecifyOnClass <- execute $ onClassChange <$> eClassChange
      let eSpecifyOnInfo = filterJust $ flip iconOnInfoChange <$> bSpecify <@> eInfoChange
          -- eSpecifyOnClass takes priority.
          -- Not that it matters, as the two events are disjoint.
          eSpecify = unionWith const eSpecifyOnClass eSpecifyOnInfo
      bSpecify <- stepper initSpecify eSpecify
  pure bSpecify
  where
    onClassChange info = liftIO . unlift $ iconOnClassChange setup appInfoCol info.windowClasses info
    branchClassChange oldInfo newInfo =
      if newInfo.windowClasses /= oldInfo.windowClasses
        then Left newInfo
        else Right newInfo

-- | On class change, re-evaluates from front to back.
-- Always invoke icon update.
iconOnClassChange ::
  WindowSetup ->
  AppInfoCol ->
  V.Vector T.Text ->
  WindowInfo ->
  PulpIO IconSpecify
iconOnClassChange WindowSetup{..} appCol winClasses winInfo = withRunInIO $ \unlift -> do
  spec <-
    runMaybeT . getAlt . foldMap Alt $
      [ mapMaybeT unlift (FromAppIcon <$> appInfoImgSetter appCol winClasses)
      , (MaybeT . pure) (FromCustom winInfo <$> windowImgIcon winClasses)
      ]
  pure $ fromMaybe FromEWMH spec

-- | Window info change without class change.
-- Handling depends on type, may not update.
iconOnInfoChange :: WindowInfo -> IconSpecify -> Maybe IconSpecify
iconOnInfoChange newInfo = \case
  -- App Icon is static.
  FromAppIcon _ -> Nothing
  -- Always update to match window info.
  FromCustom _ getIcon -> Just $ FromCustom newInfo getIcon
  -- Never updates EWMH specify depending on window info.
  FromEWMH -> Nothing

applySpecifiedIcon ::
  IO [Gtk.RawIcon] ->
  WinView.View ->
  IconSpecify ->
  IO ()
applySpecifiedIcon getRaw view = \case
  FromAppIcon icon -> WinView.setGIcon view icon
  FromCustom wInfo getIcon -> WinView.setGIcon view =<< getIcon wInfo
  FromEWMH -> WinView.setRawIcons view =<< getRaw

-- Wraps the raw icon getter so that exception is cared for.
wrapGetRaw :: GetXIcon -> PulpIO [Gtk.RawIcon]
wrapGetRaw getXIcon =
  liftIO getXIcon >>= \case
    -- MAYBE Warning level?
    Left err -> [] <$ logS logName LevelDebug (logStrf "Cannot recognize icon due to: $1" err)
    Right icons -> pure icons

{-
  TODO Consider: Log desktop changes?
  unlift $ logS (T.pack logName) LevelDebug $ logStrf "[$1] desktop $2 -> $3" windowId deskOld deskNew
-}

{-------------------------------------------------------------------
                          Application Info
--------------------------------------------------------------------}

appInfoImgSetter :: AppInfoCol -> V.Vector T.Text -> MaybeT PulpIO Gio.Icon
appInfoImgSetter appCol classes = do
  allDat <- liftIO $ getAppInfos appCol
  let findWith matcher = V.find (\dat -> any (matcher dat) classes) allDat
  appDat@AppInfoData{appId} <- MaybeT . pure $ findWith classMatch <|> findWith identMatch <|> findWith execMatch
  lift $ logS logName LevelDebug $ logStrf "AppInfo: ($1) -> ($2)" (show classes) appId
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
  { desktopStats :: Steps (V.Vector DesktopStat)
  , windowsList :: Steps (V.Vector Window)
  , windowActive :: Steps (Maybe Window)
  , trackWinInfo :: Window -> IO (Maybe PerWinRcvs)
  , reqActivate :: Window -> IO ()
  , reqToDesktop :: Int -> IO ()
  }

data PerWinRcvs = PerWinRcvs
  { winDesktop :: !(Steps Int)
  , winInfo :: !(Steps WindowInfo)
  , getWinIcon :: !GetXIcon
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
    queryWinIcon <- lift . xQueryOnce $ \() -> xOnWindow window (runXQuery getWindowIcon)
    let getWinIcon = first (formatXQError window) <$> queryWinIcon ()
    pure PerWinRcvs{..}

  reqActivate <- reqActiveWindow True
  reqToDesktop <- reqCurrentDesktop

  pure DeskVisRcvs{..}
  where
    eitherMay = either (const Nothing) Just
    onError window err = do
      liftIO (fail $ formatXQError window err)
    errorAct act = do
      window <- xWindow
      act >>= either (onError window) pure
