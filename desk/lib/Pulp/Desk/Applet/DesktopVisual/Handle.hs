{-# LANGUAGE RecursiveDo #-}

module Pulp.Desk.Applet.DesktopVisual.Handle (
  NumWindows,
  GetXIcon,
  DesktopSetup (..),
  WindowSetup (..),
  deskVisualizer,
) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Maybe
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable
import Data.GI.Base.Constructible qualified as GI
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Monoid
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import GI.Gio.Interfaces.AppInfo qualified as Gio
import GI.Gio.Interfaces.Icon qualified as Gio
import Graphics.X11.Types qualified as X11
import Pulp.Desk.Applet.DesktopVisual.DesktopItemView qualified as DeskView
import Pulp.Desk.Applet.DesktopVisual.DesktopVisual qualified as MainView
import Pulp.Desk.Applet.DesktopVisual.WindowItemView qualified as WinView
import Pulp.Desk.Env.PulpEnv
import Pulp.Desk.Reactive.Entry
import Pulp.Desk.Reactive.State
import Pulp.Desk.System.AppInfos
import Pulp.Desk.System.X11.WMStatus qualified as X11
import Pulp.Desk.System.X11.XHandle qualified as X11
import Pulp.Desk.UI.Commons qualified as Gtk
import Pulp.Desk.UI.Pixbufs qualified as Gtk
import Pulp.Desk.UI.Reactive qualified as Gtk
import Pulp.Desk.Utils.LogPrint
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

type NumWindows = Word

-- FIXME Consider that in XMonad, "Workspace" has String ID.
-- In fact, index notion of XMonad is not static (consult ewmh and its sort) - Must be fixed!

-- | Desktop part of the setup.
data DesktopSetup = DesktopSetup
  { desktopLabeling :: Maybe T.Text -> T.Text
  -- ^ Labeling rule for the desktop.
  , showDesktop :: X11.DesktopStat -> NumWindows -> Bool
  -- ^ Whether to show certain desktop or not.
  }

-- | Window part of the setup.
newtype WindowSetup = WindowSetup
  { windowImgIcon :: V.Vector T.Text -> Maybe (X11.WindowInfo -> IO Gio.Icon)
  -- ^ With which icon the window is going to set to.
  -- Whether to go with this one or not is only dependent on the window class.
  }

logName = T.pack "DeskVis"

deskVisualizer :: DesktopSetup -> WindowSetup -> PulpIO Gtk.Widget
deskVisualizer deskSetup winSetup = withRunInIO $ \unlift -> do
  DeskVisRcvs{..} <- unlift $ X11.runXHook deskVisInitiate
  appInfoCol <- trackAppInfo
  -- UI thread here
  mainView <- GI.new MainView.AsView []

  let updateDesk = updateDesktop deskSetup (Gtk.uiCreate $ GI.new DeskView.AsView [])
      updateWin = updateWindow unlift winSetup appInfoCol (Gtk.uiCreate $ GI.new WinView.AsView []) trackWinInfo

  -- For widget initialization, we assume window/desktop list starts empty,
  -- and perform initialization "update".
  (initSource, initialize) <- sourceSink
  -- TODO Require forkIO on network setup
  forkIO $ do
    network <- compile $ do
      eInit <- sourceEvent initSource

      eDesktopList <- stepsEvent eInit desktopStats
      eWindowList <- stepsEvent eInit windowsList
      bActiveWindow <- stepsBehavior windowActive

      (eDesktops, bDesktops) <- desktopList updateDesk eDesktopList
      (eWindows, bWindows) <- windowMap updateWin eWindowList
      let bDeskViews = V.map (\desk -> desk.view) <$> bDesktops
          bWinViews = M.map (\win -> win.view) <$> bWindows
          -- Can change when window list changes, for window list change could lag behind.
          -- The Maybe-bind is intended, for the view might not exist yet.
          bActiveView = (\views win -> (views M.!?) =<< win) <$> bWinViews <*> bActiveWindow

      -- Behavior updates at next step of eWindows, in sync with bWinView
      bWinDeskPairs <- switchB (pure S.empty) (asWinDeskPairs <$> eWindows)
      let bWinDeskPairMap = asViewPairMap <$> bDeskViews <*> bWinViews <*> bWinDeskPairs

      -- Compute and apply container differences.
      syncBehaviorDiff (-->) mempty (AsCacheStack <$> bDeskViews) (applyDeskDiff mainView)
      syncBehaviorDiff (-->) mempty bWinDeskPairMap applyPairDiff

      -- Sync with activated window.
      syncBehaviorDiff (-->) Nothing bActiveView applyActiveWindow

      -- Window list update changes window order in batch. We do not care of desktop changes.
      reactimate $ applyWindowPriority <$> bDesktops <@> eWindows

      -- Visibility depends on both desktop state and window count.
      let deskWithWinCnt = pairDeskCnt <$> bWinDeskPairs <*> bDesktops
      syncBehavior deskWithWinCnt $ traverse_ (uncurry $ applyDesktopVisible deskSetup)

      -- Activations to send to X11.
      eDesktopActivate <- switchE never (desktopActivates <$> eDesktops)
      eWindowActivate <- switchE never (windowActivates <$> eWindows)

      reactimate $ traverse_ reqToDesktop <$> eDesktopActivate
      reactimate $ traverse_ reqActivate <$> eWindowActivate

    actuate network
    initialize ()

  Gtk.toWidget mainView
  where
    asWinDeskPairs :: M.Map X11.Window WinItem -> Behavior (S.Set (X11.Window, Int))
    asWinDeskPairs = fmap (S.fromList . M.toList) . traverse (\desk -> desk.bWindowDesktop)

    -- Pair with desktop counts.
    pairDeskCnt :: S.Set (X11.Window, Int) -> V.Vector a -> V.Vector (NumWindows, a)
    pairDeskCnt pairMap = V.imap $ \i v -> (fromMaybe 0 (deskHisto IM.!? i), v)
      where
        pairToCnt (_win, desk) = (desk, 1 :: NumWindows)
        deskHisto = IM.fromListWith (+) . map pairToCnt $ S.toList pairMap

data DeskItem = DeskItem
  { view :: !DeskView.View
  , desktopStat :: !X11.DesktopStat
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
  M.Map X11.Window WinView.View ->
  S.Set (X11.Window, Int) ->
  CacheMap (X11.Window, Int) ViewPair
asViewPairMap desktops windows = AsCacheMap . M.mapMaybe pairToView . M.fromSet id
  where
    pairToView (win, desk) = (windows M.! win,) <$> (desktops V.!? desk)

-- Uses list since not so many events occur simultaneously
desktopActivates :: V.Vector DeskItem -> Event [Int]
desktopActivates = fold . V.imap eWithIdx
  where
    eWithIdx idx DeskItem{eDesktopClick} = [idx] <$ eDesktopClick

windowActivates :: M.Map X11.Window WinItem -> Event [X11.Window]
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
  (Maybe DeskItem -> X11.DesktopStat -> MomentIO DeskItem) ->
  Event (V.Vector X11.DesktopStat) ->
  MomentIO (Event (V.Vector DeskItem), Behavior (V.Vector DeskItem))
desktopList update eStat = do
  exeAccumD V.empty (updateFn <$> eStat)
  where
    updateFn stats olds = do
      news <- zipToRightM update olds stats
      news <$ onRemainderRight (\desk -> liftIO desk.delete) news olds

updateDesktop ::
  DesktopSetup ->
  IO (MVar DeskView.View) ->
  Maybe DeskItem ->
  X11.DesktopStat ->
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
  (Maybe WinItem -> (X11.Window, Int) -> MomentIO (Maybe WinItem)) ->
  Event (V.Vector X11.Window) ->
  MomentIO (Event (M.Map X11.Window WinItem), Behavior (M.Map X11.Window WinItem))
windowMap update eList = do
  exeAccumD M.empty (updateFn . asMapWithIdx <$> eList)
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
  (X11.Window -> IO (Maybe PerWinRcvs)) ->
  Maybe WinItem ->
  (X11.Window, Int) ->
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
applyDesktopState DesktopSetup{..} DeskItem{..} = do
  DeskView.setLabel view (desktopLabeling desktopStat.desktopName)
  DeskView.setState view desktopStat.desktopState

applyDesktopVisible :: DesktopSetup -> NumWindows -> DeskItem -> IO ()
applyDesktopVisible DesktopSetup{..} numWin DeskItem{..} = do
  DeskView.setVisible view (showDesktop desktopStat numWin)

applyWindowState :: WinView.View -> X11.WindowInfo -> IO ()
applyWindowState view X11.WindowInfo{..} = do
  WinView.setTitle view windowTitle
  WinView.setStates view (S.toList windowState)

-- From EWMH is the default
data IconSpecify = FromAppIcon !Gio.Icon | FromCustom !X11.WindowInfo (X11.WindowInfo -> IO Gio.Icon) | FromEWMH

-- | Icon specifier event stream to update the icon.
iconSpecifier ::
  (forall a. PulpIO a -> IO a) ->
  WindowSetup ->
  AppInfoCol ->
  InitedEvent X11.WindowInfo ->
  MomentIO (Behavior IconSpecify)
iconSpecifier unlift setup appInfoCol iWinInfo = do
  bWinInfo <- initedBehavior iWinInfo -- Tracks old WinInfo
  let (eClassChange, eInfoChange) = split $ branchClassChange <$> bWinInfo <@> iWinInfo.update

  rec initSpecify <- onClassChange iWinInfo.initial
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
  X11.WindowInfo ->
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
iconOnInfoChange :: X11.WindowInfo -> IconSpecify -> Maybe IconSpecify
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
  { desktopStats :: Steps (V.Vector X11.DesktopStat)
  , windowsList :: Steps (V.Vector X11.Window)
  , windowActive :: Steps (Maybe X11.Window)
  , trackWinInfo :: X11.Window -> IO (Maybe PerWinRcvs)
  , reqActivate :: X11.Window -> IO ()
  , reqToDesktop :: Int -> IO ()
  }

data PerWinRcvs = PerWinRcvs
  { winDesktop :: !(Steps Int)
  , winInfo :: !(Steps X11.WindowInfo)
  , getWinIcon :: !GetXIcon
  }

-- | Desktop visualizer event handle initiate.
--
-- The widget holds one entire X event loop,
-- so related resources will be disposed of on kill.
deskVisInitiate :: X11.XIO DeskVisRcvs
deskVisInitiate = do
  rootWin <- X11.xWindow

  desktopStats <- errorAct $ X11.watchXQuery rootWin X11.getDesktopStat pure
  windowsList <- errorAct $ X11.watchXQuery rootWin X11.getAllWindows pure
  windowActive <- errorAct $ X11.watchXQuery rootWin X11.getActiveWindow pure

  -- MAYBE log warning when window without the info is detected?
  trackWinInfo <- X11.xQueryOnce $ \window -> fmap eitherMay . runExceptT $ do
    winDesktop <- ExceptT $ X11.watchXQuery window X11.getWindowDesktop pure
    winInfo <- ExceptT $ X11.watchXQuery window X11.getWindowInfo pure
    queryWinIcon <- lift . X11.xQueryOnce $ \() -> X11.xOnWindow window (X11.runXQuery X11.getWindowIcon)
    let getWinIcon = first (X11.formatXQError window) <$> queryWinIcon ()
    pure PerWinRcvs{..}

  reqActivate <- X11.reqActiveWindow True
  reqToDesktop <- X11.reqCurrentDesktop

  pure DeskVisRcvs{..}
  where
    eitherMay = either (const Nothing) Just
    onError window err = do
      liftIO (fail $ X11.formatXQError window err)
    errorAct act = do
      window <- X11.xWindow
      act >>= either (onError window) pure
