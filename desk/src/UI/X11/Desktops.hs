-- | Desktops visualizer widget.
module UI.X11.Desktops where

import Control.Concurrent.STM
import Control.Concurrent.Task
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Foldable
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
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

deskVisNew :: MonadIO m => Display -> Window -> (WindowInfo -> IO ImageSet) -> m UI.Widget
deskVisNew display window imgSet = liftIO . runXWith display window $
  withRunInIO $ \unlifts -> do
    DeskVisRcvs{mappedWindows} <- unlifts $ xRunLoop deskVisInitiate
    allWindows <- readTVarIO mappedWindows
    winUIs <- M.traverseWithKey (deskVWindow imgSet) allWindows
    _dvWindows <- newTVarIO winUIs
    undefined

deskCssClass :: DesktopState -> T.Text
deskCssClass = \case
  DeskActive -> T.pack "active"
  DeskVisible -> T.pack "visible"
  DeskHidden -> T.pack "hidden"

windowCssClass :: WMStateEx -> T.Text
windowCssClass = \case
  WinHidden -> T.pack "hidden"
  WinDemandAttention -> T.pack "demanding"

-- | Create desktop UI parts except for windows.
deskVisual :: MonadIO m => IORef (V.Vector DesktopUI) -> Task (V.Vector DesktopStat) -> m UI.Widget
deskVisual desksRef statTask = do
  deskVis <- UI.boxNew UI.OrientationHorizontal 5
  _ <- UI.onWidgetRealize deskVis $ do
    kill <- UI.uiTask statTask (updateDesks desksRef)
    () <$ UI.onWidgetUnrealize deskVis kill
  UI.toWidget deskVis

-- | Recreate/Update the desktop UIs.
updateDesks :: IORef (V.Vector DesktopUI) -> V.Vector DesktopStat -> IO ()
updateDesks desksRef desks = do
  oldDesks <- readIORef desksRef
  -- Returns old/new commons, while writing added portion to desksRef.
  -- Was trying to maximize code reuse.
  -- TODO Factor pure parts of this one out.
  (oldCom, newCom) <- case V.length desks `compare` V.length oldDesks of
    LT -> do
      -- Less desktops, remove UIs
      let (keeps, discards) = V.splitAt (V.length desks) oldDesks
      traverse_ destroyDeskUI discards
      (keeps, desks) <$ writeIORef desksRef V.empty
    GT -> do
      -- More desktops, add UIs
      let (persists, adds) = V.splitAt (V.length oldDesks) desks
      added <- traverse newDeskUI adds
      (oldDesks, persists) <$ writeIORef desksRef added
    EQ -> (oldDesks, desks) <$ writeIORef desksRef V.empty

  -- Update the common part of UI, and then add common part back to the desksRef.
  uiCom <- V.zipWithM updateDeskUI oldCom newCom
  modifyIORef' desksRef (uiCom <>)
  where
    -- Random newline for no reason
    newDeskUI DesktopStat{..} = do
      deskBox <- UI.boxNew UI.OrientationHorizontal 5
      deskName <- UI.labelNew desktopName
      -- TODO Button press -> activate
      UI.containerAdd deskBox deskName
      desktopUI <- UI.toWidget deskBox
      updateDeskState desktopState desktopUI
      pure DesktopUI{desktopUI, deskUIName = desktopName}

    destroyDeskUI DesktopUI{desktopUI} = UI.widgetDestroy desktopUI

    updateDeskUI dui@DesktopUI{..} dstat@DesktopStat{..}
      -- Yes, name change is worthy of destroying.
      -- Not going to be convinced otherwise, whatsoever.
      | deskUIName /= desktopName = do
        destroyDeskUI dui *> newDeskUI dstat
      | otherwise = dui <$ updateDeskState desktopState desktopUI

    -- Updates desktop CSS class.
    updateDeskState state ui = do
      ctxt <- UI.widgetGetStyleContext ui
      UI.styleContextListClasses ctxt >>= traverse_ (UI.styleContextRemoveClass ctxt)
      UI.styleContextAddClass ctxt (deskCssClass state)

data DesktopUI = DesktopUI
  { deskUIName :: Maybe T.Text
  , desktopUI :: !UI.Widget
  }

data DeskWindow = DeskWindow
  { currentAssigned :: !Int
  , windowUI :: !UI.Widget
  }

-- | Image set message.
data ImageSet = ImgSName T.Text | ImgSGIcon UI.Icon

-- | DV's subwiddget for each window.
deskVWindow :: MonadIO m => (WindowInfo -> IO ImageSet) -> Window -> Task WindowInfo -> m UI.Widget
deskVWindow setImg _window infoTask = do
  winImage <- UI.imageNew
  -- TODO Button press -> activate
  _ <- UI.onWidgetRealize winImage $ do
    kill <- UI.uiTask infoTask (updateWindow winImage)
    -- TODO Window can switch workspace
    () <$ UI.onWidgetUnrealize winImage kill
  UI.toWidget winImage
  where
    updateWindow winImage winInfo = do
      let size = fromIntegral $ fromEnum UI.IconSizeDnd
      setImg winInfo >>= \case
        ImgSName name -> UI.imageSetFromIconName winImage (Just name) size
        ImgSGIcon icon -> UI.imageSetFromGicon winImage icon size

{- Here lies communications -}

data DeskVisRcvs = DeskVisRcvs
  { mappedWindows :: TVar (M.Map Window (Task WindowInfo))
  , desktopStats :: Task (V.Vector DesktopStat)
  }

-- | Desktop visualizer event handle initiate.
deskVisInitiate :: XEventHandle DeskVisRcvs
deskVisInitiate = do
  rootWin <- xWindow

  let watchWindow window = watchXQuery window getWindowInfo
  -- For now.. let's just start empty if not successful.
  allWins <- S.fromList . maybe [] V.toList <$> runXQuery getAllWindows
  initWindows <- traverse watchWindow (M.fromSet id allWins)
  mappedWindows <- liftIO $ newTVarIO initWindows
  -- Watch for the windows Mapped/Unmapped.
  -- TODO How to listen to Window mapping and update UI respectively?
  -- Guess I could send separate event - it is huge thing, after all.
  xListenTo_ substructureNotifyMask rootWin $ \case
    MapNotifyEvent{ev_window = window} -> do
      winTask <- watchWindow window
      liftIO . atomically $ modifyTVar' mappedWindows (M.insert window winTask)
    UnmapEvent{ev_window = window} -> do
      mayTask <-
        liftIO . atomically $
          stateTVar mappedWindows (M.updateLookupWithKey (\_ _ -> Nothing) window)
      for_ mayTask $ \Task{killTask} -> liftIO killTask
    _ -> pure ()

  desktopStats <- watchXQuery rootWin getDesktopStat
  pure DeskVisRcvs{..}
