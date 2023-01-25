module System.Applet.DesktopVisual.Handle (
  NumWindows,
  GetXIcon,
  DesktopSetup (..),
  WindowSetup (..),
  deskVisualizer,
) where

import Control.Applicative
import Control.Concurrent.Task
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Maybe
import Data.Bifunctor (first)
import Data.Foldable
import Data.IORef
import Data.Map.Merge.Strict qualified as M
import Data.Map.Strict qualified as M
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
import Status.AppInfos
import Status.X11.WMStatus
import Status.X11.XHandle
import System.Applet.DesktopVisual.View qualified as View
import System.Log.LogPrint

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
  { windowImgIcon :: WindowInfo -> MaybeT IO Gio.Icon
  -- ^ With which icon the window is going to set to.
  , windowIconSize :: Gtk.IconSize
  }

-- | Desktops visualizer widget. Forks its own X11 event handler.
deskVisualizer ::
  (MonadUnliftIO m, MonadLog m, MonadXHand m) =>
  DesktopSetup ->
  WindowSetup ->
  m Gtk.Widget
deskVisualizer deskSetup winSetup = do
  rcvs <- runXHand deskVisInitiate
  deskVisualView <- View.deskVisualNew
  DeskVisHandle <- deskVisMake rcvs (deskSetup, winSetup) deskVisualView

  pure $ View.deskVisualWidget deskVisualView

-- Currently handle is empty, because no external handling is permitted.
data DeskVisHandle = DeskVisHandle

-- TODO Model scheme is not working, need to make it work with pure states.
-- ..Later. I don't have time to design a proper one
deskVisMake ::
  (MonadUnliftIO m, MonadLog m) =>
  DeskVisRcvs ->
  (DesktopSetup, WindowSetup) ->
  View.DeskVisual ->
  m DeskVisHandle
deskVisMake DeskVisRcvs{..} (deskSetup, winSetup) view = withRunInIO $ \unlift -> do
  act <- registers <$> trackAppInfo <*> newIORef V.empty <*> newIORef M.empty <*> newIORef Nothing
  unlift act
  where
    registers appCol desksRef winsRef activeRef = withRunInIO $ \unlift -> do
      killStat <- Gtk.uiTask desktopStats (unlift . updateDeskStats)
      killWinCh <- Gtk.uiTask windowsList (unlift . updateWinList)
      killActiv <- Gtk.uiTask windowActive (unlift . changeActivate)
      _ <- Gtk.onWidgetDestroy (View.deskVisualWidget view) (killStat >> killWinCh >> killActiv)
      pure DeskVisHandle
      where
        updateDeskStats :: (MonadUnliftIO m, MonadLog m) => V.Vector DesktopStat -> m ()
        updateDeskStats newStats = withRunInIO $ \unlift -> do
          -- Cut down to available desktops
          let numDesk = V.length newStats
          modifyIORef' desksRef (V.take numDesk)
          View.deskVisualCtrl view (View.CutDeskItemsTo numDesk)

          -- Add additional available desktops
          oldNum <- V.length <$> readIORef desksRef
          addeds <- for (V.drop oldNum $ V.indexed newStats) $ \(idx, stat) -> do
            deskItemView <- Glib.new View.DesktopItemView []
            View.desktopClickAct deskItemView $ reqToDesktop idx
            (deskItemView,) <$> unlift (deskItemMake deskSetup stat deskItemView)
          modifyIORef' desksRef (<> fmap snd addeds)
          View.deskVisualCtrl view (View.AddDeskItems $ fmap fst addeds)

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
          -- Little hack
          for_ (prevActive >>= (windows M.!?)) $ \WinItemHandle{itemWindowView} -> do
            View.widgetUpdateClass itemWindowView winActiveCssClass []
          for_ (nextActive >>= (windows M.!?)) $ \WinItemHandle{itemWindowView} -> do
            View.widgetUpdateClass itemWindowView winActiveCssClass [()]

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
          View.widgetUpdateClass view deskCssClass [desktopState]
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
                rawIcons <- getXIcon >>= \case
                  Left err -> unlift $ do
                    -- MAYBE Window id?
                    [] <$ logS (T.pack "DeskVis") LevelDebug (logStrf "Cannot recognize icon due to: $1" err)
                  Right icons -> pure icons
                View.windowSetRawIcons view rawIcons
            View.widgetUpdateClass view windowCssClass (S.toList windowState)

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
