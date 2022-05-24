{-# LANGUAGE MonoLocalBinds #-}

-- | Desktops visualizer widget.
module UI.X11.Desktops where

import Control.Concurrent
import Control.Concurrent.Task
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Foldable
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Tuple (swap)
import Data.Vector qualified as V
import GI.Gio.Interfaces.Icon qualified as UI
import GI.Gtk.Objects.Box qualified as UI
import GI.Gtk.Objects.Container qualified as UI
import GI.Gtk.Objects.Image qualified as UI
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types
import Status.X11.WMStatus
import Status.X11.XHandle
import UI.Commons qualified as UI
import UI.Singles qualified as UI
import UI.Task qualified as UI
import qualified GI.Gtk.Objects.Stack as UI

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

-- | Desktops visualizer widget.
-- UI is made of 2 parts, the desktops and the windows.
-- This comes from unfortunate limitation of X11.
deskVisNew :: MonadIO m => Display -> Window -> (WindowInfo -> IO ImageSet) -> m UI.Widget
deskVisNew display window setImg = liftIO . runXWith display window $
  withRunInIO $ \unlifts -> do
    DeskVisRcvs{..} <- unlifts $ xRunLoop deskVisInitiate

    deskRef <- newIORef V.empty
    let switchDesktop deskOld deskNew (winUI :: UI.Widget) = do
          desktops <- readIORef deskRef
          -- Out of bounds: was already removed, no need to care
          for_ (desktops V.!? deskOld) $ \DesktopUI{desktopUI} -> do
            UI.containerRemove desktopUI winUI
          -- For now, let's not care about out of bounds from new.
          -- That is likely a sync issue, so it needs proper logging later.
          for_ (desktops V.!? deskNew) $ \DesktopUI{desktopUI} -> do
            UI.containerAdd desktopUI winUI

    desktopVisualizer <- UI.stackNew
    deskWindows <- dVisWindowCont setImg switchDesktop windowChanges
    deskVis <- dVisDesktops deskRef desktopStats
    traverse_ (UI.containerAdd desktopVisualizer) [deskWindows, deskVis]
    UI.setStackVisibleChild desktopVisualizer deskVis
    UI.toWidget deskVis

{-------------------------------------------------------------------
                    For each desktop (workspace)
--------------------------------------------------------------------}

data DesktopUI = DesktopUI
  { desktopUI :: !UI.Container
  , deskNameChange :: Maybe T.Text -> IO ()
  }

-- | Create UI for desktops.
dVisDesktops :: MonadIO m => IORef (V.Vector DesktopUI) -> Task (V.Vector DesktopStat) -> m UI.Widget
dVisDesktops desks statTask = do
  deskVis <- UI.boxNew UI.OrientationHorizontal 5
  _ <- UI.onWidgetRealize deskVis $ do
    kill <- UI.uiTask statTask (dVisDesktopUpdate deskVis desks)
    () <$ UI.onWidgetUnrealize deskVis kill
  UI.toWidget deskVis

-- | Recreate/Update the desktops UI.
dVisDesktopUpdate :: UI.Box -> IORef (V.Vector DesktopUI) -> V.Vector DesktopStat -> IO ()
dVisDesktopUpdate parent desksRef curDesks = do
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

      let deskNameChange name = UI.labelSetLabel deskName $ maybe T.empty id name
      deskNameChange desktopName
      -- TODO Button press -> activate
      UI.containerAdd deskBox deskName
      desktopUI <- UI.toContainer deskBox

      UI.containerAdd parent desktopUI
      UI.widgetGetStyleContext desktopUI >>= updateDeskState [desktopState]
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
  m UI.Widget
dVisWindowCont setImg switcher taskWinChange = do
  placeholder <- UI.imageNew
  UI.onWidgetRealize placeholder $ do
    winsRef <- newIORef M.empty
    kill <- UI.uiTask taskWinChange (updateWindow winsRef)
    () <$ UI.onWidgetUnrealize placeholder kill
  UI.toWidget placeholder
  where
    updateWindow winsRef = \case
      DWindowMap win winTask -> do
        winUI <- dVisWindowItem setImg switcher win winTask
        modifyIORef' winsRef (M.insert win $ DeskWindowUI winTask winUI)
      DWindowUnmap win -> do
        mayWin <-
          atomicModifyIORef' winsRef $ swap . M.updateLookupWithKey (\_ _ -> Nothing) win
        for_ mayWin $ \(DeskWindowUI Task{killTask} winUI) -> do
          killTask >> UI.widgetDestroy winUI

-- | Image set message.
data ImageSet = ImgSName T.Text | ImgSGIcon UI.Icon

dVisWindowItem ::
  MonadIO m =>
  (WindowInfo -> IO ImageSet) ->
  (Int -> Int -> UI.Widget -> IO ()) ->
  Window ->
  Task WindowInfo ->
  m UI.Widget
dVisWindowItem setImg switcher _window infoTask = do
  winImage <- UI.imageNew
  UI.widgetGetStyleContext winImage >>= flip UI.styleContextAddClass (T.pack "")
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
  }

-- | Desktop visualizer event handle initiate.
--
-- The widget holds one entire X event loop,
-- so related resources will be disposed of on kill.
deskVisInitiate :: XEventHandle DeskVisRcvs
deskVisInitiate = do
  rootWin <- xWindow

  -- For now.. let's just start empty if not successful.
  allWins <- maybe [] V.toList <$> runXQuery getAllWindows

  -- Sends window map event over to UI.
  -- Let me think later about out-of-sync issue..
  winChs <- liftIO newEmptyMVar
  -- This hands the (ownership of) Task over to UI side.
  let onWindowMap window = do
        mapEvent <- DWindowMap window <$> watchXQuery window getWindowInfo
        liftIO $ putMVar winChs mapEvent
      onWindowUnmap window = liftIO $ putMVar winChs (DWindowUnmap window)
  traverse_ onWindowMap allWins
  let windowChanges = Task (pure ()) winChs

  -- Watch for the windows Mapped/Unmapped.
  xListenTo_ substructureNotifyMask rootWin $ \case
    MapNotifyEvent{ev_window = window} -> onWindowMap window
    UnmapEvent{ev_window = window} -> onWindowUnmap window
    _ -> pure ()

  desktopStats <- watchXQuery rootWin getDesktopStat
  pure DeskVisRcvs{..}
