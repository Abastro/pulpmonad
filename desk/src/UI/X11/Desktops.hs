{-# LANGUAGE MonoLocalBinds #-}

-- | Desktops visualizer widget.
module UI.X11.Desktops where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Task
import Control.Exception.Enclosed
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Monoid
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Tuple (swap)
import Data.Vector qualified as V
import GI.GLib (castTo)
import GI.Gio.Interfaces.AppInfo
import GI.Gio.Interfaces.Icon qualified as UI
import GI.Gio.Objects.DesktopAppInfo
import GI.Gtk.Objects.Box qualified as UI
import GI.Gtk.Objects.Container qualified as UI
import GI.Gtk.Objects.IconTheme qualified as UI
import GI.Gtk.Objects.Image qualified as UI
import Graphics.X11.Types
import Status.X11.WMStatus
import Status.X11.XHandle
import System.Log.Logger
import UI.Commons qualified as UI
import UI.Singles qualified as UI
import UI.Task qualified as UI
import System.IO

deskCssClass :: DesktopState -> T.Text
deskCssClass = \case
  DeskActive -> T.pack "active"
  DeskVisible -> T.pack "visible"
  DeskHidden -> T.pack "hidden"

windowCssClass :: WMStateEx -> T.Text
windowCssClass = \case
  WinHidden -> T.pack "hidden"
  WinDemandAttention -> T.pack "demanding"

-- | Update to have certain CSS classes from the state.
updateCssClass ::
  (Enum s, Bounded s, MonadIO m) => (s -> T.Text) -> [s] -> UI.StyleContext -> m ()
updateCssClass asClass state ctxt = do
  traverse_ (UI.styleContextRemoveClass ctxt) (asClass <$> [minBound .. maxBound])
  traverse_ (UI.styleContextAddClass ctxt) (asClass <$> state)

-- | Image set message.
data ImageSet = ImgSName T.Text | ImgSGIcon UI.Icon

appInfoImageSetter :: WindowInfo -> MaybeT IO ImageSet
appInfoImageSetter WindowInfo{..} = do
  icon <- getAlt $ foldMap Alt $ findIcon <$> windowClasses
  pure (ImgSGIcon icon)
  where
    findIcon className = MaybeT $ do
      allInfos <- appInfoGetAll
      deskInfos <- catMaybes <$> traverse (castTo DesktopAppInfo) allInfos
      filtered <- filterM (fmap (== Just className) . appWmClass) deskInfos
      join . listToMaybe <$> traverse appInfoGetIcon filtered
    appWmClass appInfo =
      either (const Nothing) Just
        <$> tryAny (desktopAppInfoGetStartupWmClass appInfo)

classImageSetter :: WindowInfo -> MaybeT IO ImageSet
classImageSetter WindowInfo{windowClasses} = do
  iconTheme <- UI.iconThemeGetDefault
  iconName <- getAlt $ foldMap Alt $ findIcon iconTheme <$> windowClasses
  pure (ImgSName iconName)
  where
    findIcon iconTheme className = do
      UI.iconThemeHasIcon iconTheme className >>= guard
      pure className

defImageSetter :: WindowInfo -> IO ImageSet
defImageSetter winInfo = do
  imageSet <- runMaybeT (appInfoImageSetter winInfo <|> classImageSetter winInfo)
  pure (fromMaybe (ImgSName $ T.pack "missing") imageSet)

-- | Desktops visualizer widget.
-- UI is made of 2 parts, the desktops and the windows - due to unfortunate limitation of X11.
-- Forks its own event handler.
deskVisNew ::
  MonadIO m =>
  (Maybe T.Text -> T.Text) ->
  (WindowInfo -> IO ImageSet) ->
  m UI.Widget
deskVisNew labeling setImg = do
  DeskVisRcvs{..} <- liftIO $ startXIO deskVisInitiate

  -- FIXME This verifies that the entire threading system halts.. duh
  -- Happens whenever something is closed.
  liftIO . forkIO . forever $ do
    hPutStrLn stderr "Periodic print"
    threadDelay 1000000

  deskRef <- liftIO $ newIORef V.empty
  desktopVisualizer <- UI.boxNew UI.OrientationHorizontal 0
  deskWindows <- dVisWindowCont setImg (switchDesktop deskRef) windowsChange windowActive
  deskVis <- dVisDesktopCont labeling deskRef desktopStats

  traverse_ (UI.containerAdd desktopVisualizer) [deskWindows, deskVis]
  UI.widgetShowAll desktopVisualizer
  UI.toWidget desktopVisualizer
  where
    switchDesktop deskRef deskOld deskNew DeskWindowUI{..} = liftIO $ do
      -- TODO Handle window ordering
      infoM "DeskVis" (show windowId <> ": Switching desktop from " <> show deskOld <> " to " <> show deskNew)
      desktops <- readIORef deskRef
      -- Out of bounds: was already removed, no need to care
      for_ (desktops V.!? deskOld) $ \DesktopUI{desktopUI} -> do
        UI.widgetHide windowUI
        UI.containerRemove desktopUI windowUI
      -- For now, let's not care about out of bounds from new.
      -- That is likely a sync issue, so it needs proper logging later.
      for_ (desktops V.!? deskNew) $ \DesktopUI{desktopUI} -> do
        UI.containerAdd desktopUI windowUI
        UI.widgetShowAll windowUI

-- TODO More immediate feedback on switch

{-------------------------------------------------------------------
                    For each desktop (workspace)
--------------------------------------------------------------------}

data DesktopUI = DesktopUI
  { desktopUI :: !UI.Container
  , deskNameChange :: Maybe T.Text -> IO ()
  }

-- TODO Desktop visibility

-- | Create UI for desktops.
dVisDesktopCont ::
  MonadIO m =>
  (Maybe T.Text -> T.Text) ->
  IORef (V.Vector DesktopUI) ->
  Task (V.Vector DesktopStat) ->
  m UI.Widget
dVisDesktopCont labeling desks statTask = do
  deskVis <- UI.boxNew UI.OrientationHorizontal 5
  UI.widgetGetStyleContext deskVis >>= flip UI.styleContextAddClass (T.pack "desktop-cont")
  _ <- UI.onWidgetRealize deskVis $ do
    liftIO $ infoM "DeskVis" $ "UI: Realizing Desktop Container"
    kill <- UI.uiTask statTask (dVisDesktopUpdate deskVis labeling desks)
    () <$ UI.onWidgetUnrealize deskVis kill
  UI.toWidget deskVis

-- | Recreate/Update the desktops UI.
dVisDesktopUpdate ::
  UI.Box ->
  (Maybe T.Text -> T.Text) ->
  IORef (V.Vector DesktopUI) ->
  V.Vector DesktopStat ->
  IO ()
dVisDesktopUpdate parent labeling desksRef curDesks = do
  oldDesks <- readIORef desksRef
  -- Returns old/new commons, while writing added portion to desksRef.
  -- Was trying to maximize code reuse.
  -- TODO Factor pure parts of this one out.
  (oldCom, newCom) <- case V.length curDesks `compare` V.length oldDesks of
    LT -> do
      -- Less desktops, remove UIs
      let (keeps, discards) = V.splitAt (V.length curDesks) oldDesks
      traverse_ destroyDeskUI discards
      (keeps, curDesks) <$ writeIORef desksRef V.empty
    GT -> do
      -- More desktops, add UIs
      let (persists, adds) = V.splitAt (V.length oldDesks) curDesks
      added <- traverse newDeskUI adds
      (oldDesks, persists) <$ writeIORef desksRef added
    EQ -> (oldDesks, curDesks) <$ writeIORef desksRef V.empty

  -- Update the common part of UI, and then add common part back to the desksRef.
  uiCom <- V.zipWithM updateDeskUI oldCom newCom
  modifyIORef' desksRef (uiCom <>)
  where
    newDeskUI DesktopStat{..} = do
      deskBox <- UI.boxNew UI.OrientationHorizontal 0
      deskName <- UI.labelNew Nothing
      UI.widgetGetStyleContext deskName >>= flip UI.styleContextAddClass (T.pack "desktop-label")

      let deskNameChange name = UI.labelSetLabel deskName $ labeling name
      deskNameChange desktopName
      -- TODO Button press -> activate
      UI.containerAdd deskBox deskName
      desktopUI <- UI.toContainer deskBox

      UI.widgetGetStyleContext desktopUI >>= \ctxt -> do
        UI.styleContextAddClass ctxt (T.pack "desktop-item")
        updateDeskState [desktopState] ctxt

      infoM "DeskVis" $ "UI: Desktop " <> show desktopName
      UI.containerAdd parent desktopUI
      UI.widgetShowAll desktopUI
      pure DesktopUI{..}

    destroyDeskUI DesktopUI{desktopUI} = UI.widgetDestroy desktopUI

    updateDeskUI dui@DesktopUI{..} DesktopStat{..} = do
      deskNameChange desktopName
      UI.widgetGetStyleContext desktopUI >>= updateDeskState [desktopState]
      pure dui

    updateDeskState = updateCssClass deskCssClass

{-------------------------------------------------------------------
                          For each window
--------------------------------------------------------------------}

data DeskWindowUI = DeskWindowUI
  { windowId :: Window
  , windowUI :: !UI.Widget
  }

-- | Create placeholder widget which manages window widgets.
dVisWindowCont ::
  MonadIO m =>
  (WindowInfo -> IO ImageSet) ->
  (Int -> Int -> DeskWindowUI -> IO ()) ->
  Task DWindowChange ->
  Task (Maybe Window) ->
  m UI.Widget
dVisWindowCont setImg switcher taskWinChange taskActive = do
  placeholder <- UI.imageNew
  UI.onWidgetRealize placeholder $ do
    winsRef <- newIORef M.empty
    activeRef <- newIORef Nothing
    liftIO $ infoM "DeskVis" $ "UI: Realizing Window Container"
    killUpd <- UI.uiTask taskWinChange (structureUpd winsRef)
    killAct <- UI.uiTask taskActive (changeActivate winsRef activeRef)
    () <$ UI.onWidgetUnrealize placeholder (killAct >> killUpd)

  UI.toWidget placeholder
  where
    structureUpd winsRef = \case
      DWindowChange addWins removed -> do
        -- Removes window
        for_ (S.toList removed) $ \window -> do
          mayUI <- atomicModifyIORef' winsRef $ swap . M.updateLookupWithKey (\_ _ -> Nothing) window
          for_ mayUI $ \DeskWindowUI{windowUI} -> do
            UI.widgetDestroy windowUI

        -- Adds window
        for_ (M.toList addWins) $ \(window, winTask) -> do
          existed <- (window `M.member`) <$> readIORef winsRef
          unless existed $ do
            winUI <- dVisWindowItem setImg switcher window winTask
            liftIO $ infoM "DeskVis" $ "UI: Showed Window " <> show window
            modifyIORef' winsRef (M.insert window winUI)

    changeActivate winsRef activeRef nextActive = do
      windows <- readIORef winsRef
      prevActive <- atomicModifyIORef' activeRef (nextActive,)
      let active = T.pack "active" -- Meh
      -- Little hack(?) because I am lazy
      for_ (prevActive >>= (windows M.!?)) $ \DeskWindowUI{windowUI} -> do
        UI.widgetGetStyleContext windowUI >>= flip UI.styleContextRemoveClass active
      for_ (nextActive >>= (windows M.!?)) $ \DeskWindowUI{windowUI} -> do
        UI.widgetGetStyleContext windowUI >>= flip UI.styleContextAddClass active

dVisWindowItem ::
  MonadIO m =>
  (WindowInfo -> IO ImageSet) ->
  (Int -> Int -> DeskWindowUI -> IO ()) ->
  Window ->
  Task WindowInfo ->
  m DeskWindowUI
dVisWindowItem setImg switcher _window infoTask = do
  winImage <- UI.imageNew
  UI.widgetGetStyleContext winImage >>= flip UI.styleContextAddClass (T.pack "window-item")

  -- Cannot rely on realization, since window widgets would be floating
  liftIO $ do
    curDesk <- newIORef @Int (-1) -- When desktop is -1, it should not be showed.
    kill <- UI.uiTask infoTask (updateWindow winImage curDesk)
    UI.onWidgetDestroy winImage kill

  DeskWindowUI _window <$> UI.toWidget winImage
  where
    updateWindow winImage curDesk winInfo@WindowInfo{..} = do
      let newDesk = windowDesktop
      -- Switches the desktop
      oldDesk <- atomicModifyIORef' curDesk $ (newDesk,)
      when (newDesk /= oldDesk) $
        UI.toWidget winImage >>= switcher oldDesk newDesk . DeskWindowUI _window

      -- Update the icon
      let size = fromIntegral $ fromEnum UI.IconSizeLargeToolbar
      setImg winInfo >>= \case
        -- MAYBE use pixel size to reset.
        ImgSName name -> UI.imageSetFromIconName winImage (Just name) size
        ImgSGIcon icon -> UI.imageSetFromGicon winImage icon size

      -- Update the class
      UI.widgetGetStyleContext winImage >>= updateCssClass windowCssClass (S.toList windowState)

{-------------------------------------------------------------------
                        Communication
--------------------------------------------------------------------}

-- | Window changes, with added & removed
data DWindowChange = DWindowChange (M.Map Window (Task WindowInfo)) (S.Set Window)

-- data DVWindowRcv = DWindowMap Window (Task WindowInfo) | DWindowUnmap Window

data DeskVisRcvs = DeskVisRcvs
  { desktopStats :: Task (V.Vector DesktopStat)
  , windowsChange :: Task DWindowChange
  , windowActive :: Task (Maybe Window)
  }

-- | Desktop visualizer event handle initiate.
--
-- The widget holds one entire X event loop,
-- so related resources will be disposed of on kill.
deskVisInitiate :: XIO () DeskVisRcvs
deskVisInitiate = do
  rootWin <- xWindow

  liftIO $ infoM "DeskVis" "DeskVis Initiating..."

  -- Reference to hold current data of windows.
  -- IORef, since X11 handling should be happening on the same thread.
  windowRef <- liftIO $ newIORef S.empty
  -- Detects difference between windows and handle it.
  windowsChange <- errorAct $
    watchXQuery rootWin getAllWindows $ \newWindows -> do
      oldWindows <- liftIO $ atomicModifyIORef' windowRef $ \oldWindows -> (newWindows, oldWindows)
      let removed = oldWindows S.\\ newWindows
      let added = newWindows S.\\ oldWindows
      -- "Invalid" windows are ignored from here
      addWins <- M.traverseMaybeWithKey (\_ -> lift . watchWindow) (M.fromSet id added)
      pure $ DWindowChange addWins removed

  windowActive <- errorAct $ watchXQuery rootWin getActiveWindow pure
  -- MAYBE Also implement desktop messages
  desktopStats <- errorAct $ watchXQuery rootWin getDesktopStat pure

  liftIO $ infoM "DeskVis" "DeskVis Initiated."

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
