{-# LANGUAGE MonoLocalBinds #-}

-- | Desktops visualizer widget.
module UI.X11.Desktops where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Task
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
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
import Graphics.X11.Xlib.Extras
import Status.X11.WMStatus
import Status.X11.XHandle
import System.IO
import UI.Commons qualified as UI
import UI.Singles qualified as UI
import UI.Task qualified as UI

-- FIXME !! Proper logging !!
-- Some kind of slowdown/deadlock is still happening somewhere

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
        <$> try @IOException (desktopAppInfoGetStartupWmClass appInfo)

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

  deskRef <- liftIO $ newIORef V.empty
  let switchDesktop deskOld deskNew (winUI :: UI.Widget) = liftIO $ do
        desktops <- readIORef deskRef
        -- Out of bounds: was already removed, no need to care
        for_ (desktops V.!? deskOld) $ \DesktopUI{desktopUI} -> do
          hPutStrLn stderr "Removing window UI"
          UI.containerRemove desktopUI winUI
          hPutStrLn stderr "Removed window UI"
        -- For now, let's not care about out of bounds from new.
        -- That is likely a sync issue, so it needs proper logging later.
        for_ (desktops V.!? deskNew) $ \DesktopUI{desktopUI} -> do
          hPutStrLn stderr "Adding window UI"
          UI.containerAdd desktopUI winUI
          hPutStrLn stderr "Added window UI"

  desktopVisualizer <- UI.boxNew UI.OrientationHorizontal 0
  deskWindows <- dVisWindowCont setImg switchDesktop windowChanges windowActive
  deskVis <- dVisDesktopCont labeling deskRef desktopStats

  traverse_ (UI.containerAdd desktopVisualizer) [deskWindows, deskVis]
  liftIO $ hPutStrLn stderr "Visualizer booted up"
  UI.widgetShowAll desktopVisualizer
  UI.toWidget desktopVisualizer

{-------------------------------------------------------------------
                    For each desktop (workspace)
--------------------------------------------------------------------}

data DesktopUI = DesktopUI
  { desktopUI :: !UI.Container
  , deskNameChange :: Maybe T.Text -> IO ()
  }

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
    hPutStrLn stderr "Realizing desktop container"
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
      deskBox <- UI.boxNew UI.OrientationHorizontal 5
      deskName <- UI.labelNew Nothing
      UI.widgetGetStyleContext deskName >>= flip UI.styleContextAddClass (T.pack "desktop-label")

      let deskNameChange name = UI.labelSetLabel deskName $ labeling name
      deskNameChange desktopName
      -- TODO Button press -> activate
      UI.containerAdd deskBox deskName
      desktopUI <- UI.toContainer deskBox

      hPutStrLn stderr $ "Adding desktop UI, named: " <> show desktopName
      UI.containerAdd parent desktopUI
      hPutStrLn stderr "Added desktop UI"
      UI.widgetGetStyleContext desktopUI >>= \ctxt -> do
        UI.styleContextAddClass ctxt (T.pack "desktop-item")
        updateDeskState [desktopState] ctxt
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
  { windowTask :: Task WindowInfo
  , windowUI :: !UI.Widget
  }

-- | Create placeholder widget which manages window widgets.
dVisWindowCont ::
  MonadIO m =>
  (WindowInfo -> IO ImageSet) ->
  (Int -> Int -> UI.Widget -> IO ()) ->
  Task DVWindowRcv ->
  Task (Maybe Window) ->
  m UI.Widget
dVisWindowCont setImg switcher taskWinChange taskActive = do
  placeholder <- UI.imageNew
  UI.onWidgetRealize placeholder $ do
    hPutStrLn stderr "Realizing window container"
    winsRef <- newIORef M.empty
    activeRef <- newIORef Nothing
    killUpd <- UI.uiTask taskWinChange (structureUpd winsRef)
    killAct <- UI.uiTask taskActive (changeActivate winsRef activeRef)
    () <$ UI.onWidgetUnrealize placeholder (killAct >> killUpd)

  UI.toWidget placeholder
  where
    structureUpd winsRef = \case
      DWindowMap win winTask -> do
        liftIO $ hPutStrLn stderr $ "Window mapping: " <> show win
        winUI <- dVisWindowItem setImg switcher win winTask
        modifyIORef' winsRef (M.insert win $ DeskWindowUI winTask winUI)
      DWindowUnmap win -> do
        liftIO $ hPutStrLn stderr $ "Window unmapping: " <> show win
        mayWin <-
          atomicModifyIORef' winsRef $ swap . M.updateLookupWithKey (\_ _ -> Nothing) win
        for_ mayWin $ \(DeskWindowUI Task{killTask} winUI) -> do
          killTask >> UI.widgetDestroy winUI

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
  (Int -> Int -> UI.Widget -> IO ()) ->
  Window ->
  Task WindowInfo ->
  m UI.Widget
dVisWindowItem setImg switcher _window infoTask = do
  winImage <- UI.imageNew
  UI.widgetGetStyleContext winImage >>= flip UI.styleContextAddClass (T.pack "window-item")
  -- TODO Button press -> activate
  _ <- UI.onWidgetRealize winImage $ do
    curDesk <- newIORef @Int (-1) -- Leveraging that it should not be -1.
    kill <- UI.uiTask infoTask (updateWindow winImage curDesk)
    () <$ UI.onWidgetUnrealize winImage kill
  UI.toWidget winImage
  where
    updateWindow winImage curDesk winInfo@WindowInfo{..} = do
      let newDesk = windowDesktop
      -- Switches the desktop
      oldDesk <- atomicModifyIORef' curDesk $ (newDesk,)
      UI.toWidget winImage >>= switcher oldDesk newDesk

      -- Update the icon
      let size = fromIntegral $ fromEnum UI.IconSizeDnd
      setImg winInfo >>= \case
        ImgSName name -> UI.imageSetFromIconName winImage (Just name) size
        ImgSGIcon icon -> UI.imageSetFromGicon winImage icon size

      -- Update the class
      UI.widgetGetStyleContext winImage >>= updateCssClass windowCssClass (S.toList windowState)

{-------------------------------------------------------------------
                        Communication
--------------------------------------------------------------------}

data DVWindowRcv = DWindowMap Window (Task WindowInfo) | DWindowUnmap Window

data DeskVisRcvs = DeskVisRcvs
  { desktopStats :: Task (V.Vector DesktopStat)
  , windowChanges :: Task DVWindowRcv
  , windowActive :: Task (Maybe Window)
  }

-- | Desktop visualizer event handle initiate.
--
-- The widget holds one entire X event loop,
-- so related resources will be disposed of on kill.
deskVisInitiate :: XIO () DeskVisRcvs
deskVisInitiate = do
  rootWin <- xWindow

  -- For now.. let's just start empty if not successful.
  allWins <- maybe [] V.toList <$> runXQuery getAllWindows

  liftIO $ hPutStrLn stderr "Initiate Begin"

  -- Sends window map event over to UI.
  -- Let me think later about out-of-sync issue..
  winChs <- liftIO newEmptyMVar
  -- This hands the (ownership of) Task over to UI side.
  let onWindowMap window = do
        mapEvent <- DWindowMap window <$> watchXQuery window getWindowInfo
        liftIO $ hPutStrLn stderr $ "Mapping " <> show window
        liftIO $ putMVar winChs mapEvent
        liftIO $ hPutStrLn stderr $ "Mapped " <> show window
      onWindowUnmap window = liftIO $ putMVar winChs (DWindowUnmap window)
  let windowChanges = Task (pure ()) winChs

  -- FIXME This is bad. Better way?
  xQueueJob $ traverse_ onWindowMap allWins
  -- Watch for the windows Mapped/Unmapped.
  xListenTo_ substructureNotifyMask rootWin $ \case
    MapNotifyEvent{ev_window = window} -> onWindowMap window
    UnmapEvent{ev_window = window} -> onWindowUnmap window
    _ -> pure ()

  windowActive <- watchXQuery rootWin getActiveWindow
  desktopStats <- watchXQuery rootWin getDesktopStat

  liftIO $ hPutStrLn stderr "Initiate End"
  pure DeskVisRcvs{..}
