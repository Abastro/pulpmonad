{-# LANGUAGE MonoLocalBinds #-}

-- | Desktops visualizer widget.
module UI.X11.DesktopVisual (
  DesktopSetup(..),
  WindowSetup(..),
  deskVisualizerNew,
  defImageSetter,
  appInfoImageSetter,
  classImageSetter,
  defShowFn,
) where

import Control.Applicative
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
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gio.Objects.DesktopAppInfo
import GI.Gtk.Objects.Box qualified as UI
import GI.Gtk.Objects.IconTheme qualified as UI
import GI.Gtk.Objects.Image qualified as UI
import Graphics.X11.Types
import Status.X11.WMStatus
import Status.X11.XHandle
import System.Log.Logger
import UI.Commons qualified as UI
import UI.Containers qualified as UI
import UI.Singles qualified as UI
import UI.Styles qualified as UI
import UI.Task qualified as UI

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

-- | Image set message.
data ImageSet = ImgSName T.Text | ImgSGIcon Gio.Icon

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

type NumWindows = Word

defShowFn :: DesktopStat -> NumWindows -> Bool
defShowFn DesktopStat{desktopState} num = desktopState /= DeskHidden || num > 0

-- | Desktop part of the setup.
data DesktopSetup = DesktopSetup
  { desktopLabeling :: Maybe T.Text -> T.Text
  -- ^ Labeling rule for the desktop.
  , showDesktop :: DesktopStat -> NumWindows -> Bool
  -- ^ Whether to show certain desktop or not.
  }

-- | Window part of the setup.
data WindowSetup = WindowSetup
  { windowImgSetter :: WindowInfo -> IO ImageSet
  -- ^ With which iamge the window is going to set to.
  }

-- | Desktops visualizer widget.
--
-- UI is made of 2 parts, the desktops and the windows,
-- due to unfortunate (or not) limitation of X11.
-- Forks its own event handler.
deskVisualizerNew ::
  MonadIO m => DesktopSetup -> WindowSetup -> m UI.Widget
deskVisualizerNew deskSetup winSetup = do
  DeskVisRcvs{..} <- liftIO $ startXIO deskVisInitiate

  deskRef <- liftIO $ newIORef V.empty
  desktopVisualizer <- UI.boxNew UI.OrientationHorizontal 0
  deskWindows <- dVisWindowCont winSetup (switchDesktop deskRef) reqActivate windowsChange windowActive
  deskVis <- dVisDesktopCont deskSetup deskRef desktopStats

  traverse_ (UI.containerAdd desktopVisualizer) [deskWindows, deskVis]
  UI.widgetShowAll desktopVisualizer
  UI.toWidget desktopVisualizer
  where
    switchDesktop deskRef deskOld deskNew DeskWindowUI{..} = liftIO $ do
      -- TODO Handle window ordering
      infoM "DeskVis" (show windowId <> ": Switching desktop from " <> show deskOld <> " to " <> show deskNew)
      desktops <- readIORef deskRef
      -- Out of bounds: was already removed, no need to care
      for_ (desktops V.!? deskOld) $ \DesktopUI{..} -> deskAddRemove False windowUI
      -- For now, let's not care about out of bounds from new windows.
      -- That is likely a sync issue, so it needs proper logging later.
      for_ (desktops V.!? deskNew) $ \DesktopUI{..} -> deskAddRemove True windowUI

-- TODO More immediate feedback on switch

{-------------------------------------------------------------------
                    For each desktop (workspace)
--------------------------------------------------------------------}

data DesktopUI = DesktopUI
  { desktopUI :: !UI.Container
  , deskNameChange :: Maybe T.Text -> IO ()
  , deskAddRemove :: Bool -> UI.Widget -> IO ()
  -- ^ True for add, False for remove
  , curDeskStat :: !(IORef DesktopStat) -- TODO Not ideal
  }

-- | Create UI for desktops.
dVisDesktopCont ::
  MonadIO m =>
  DesktopSetup ->
  IORef (V.Vector DesktopUI) ->
  Task (V.Vector DesktopStat) ->
  m UI.Widget
dVisDesktopCont deskSetup desks statTask = do
  -- !!! Leaky abstraction !!!
  deskVis <- UI.boxNew UI.OrientationHorizontal 5
  UI.widgetGetStyleContext deskVis >>= flip UI.styleContextAddClass (T.pack "desktop-cont")
  liftIO $ do
    kill <- UI.uiTask statTask (dVisDesktopUpdate deskVis deskSetup desks)
    () <$ UI.onWidgetDestroy deskVis kill
  UI.toWidget deskVis

-- | Recreate/Update the desktops UI.
dVisDesktopUpdate ::
  UI.Box ->
  DesktopSetup ->
  IORef (V.Vector DesktopUI) ->
  V.Vector DesktopStat ->
  IO ()
dVisDesktopUpdate parent DesktopSetup{..} desksRef curDesks = do
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
    newDeskUI deskStat@DesktopStat{..} = do
      deskBox <- UI.boxNew UI.OrientationHorizontal 0
      deskName <- UI.labelNew Nothing
      UI.widgetGetStyleContext deskName >>= flip UI.styleContextAddClass (T.pack "desktop-label")

      let deskNameChange name = UI.labelSetLabel deskName $ desktopLabeling name
      -- TODO Button press -> activate
      UI.containerAdd deskBox deskName
      desktopUI <- UI.toContainer deskBox
      UI.widgetGetStyleContext desktopUI >>= flip UI.styleContextAddClass (T.pack "desktop-item")

      infoM "DeskVis" $ "UI: Desktop " <> show desktopName

      curDeskStat <- newIORef deskStat
      let deskUI = DesktopUI{deskAddRemove = deskUIAddRemove deskUI, ..}
      updateDeskUI deskUI deskStat
      UI.containerAdd parent desktopUI
      pure deskUI

    destroyDeskUI DesktopUI{desktopUI} = UI.widgetDestroy desktopUI

    updateDeskUI deskUI@DesktopUI{..} deskStat@DesktopStat{..} = do
      deskNameChange desktopName
      UI.widgetGetStyleContext desktopUI >>= UI.updateCssClass deskCssClass [desktopState]
      writeIORef curDeskStat deskStat
      updateVisible deskUI
      pure deskUI

    updateVisible DesktopUI{..} = do
      deskStat <- readIORef curDeskStat
      numWindows <- fromIntegral . pred . length <$> UI.containerGetChildren desktopUI
      case showDesktop deskStat numWindows of
        True -> UI.widgetShowAll desktopUI
        False -> UI.widgetHide desktopUI

    deskUIAddRemove deskUI@DesktopUI{desktopUI} flag widget = case flag of
      True -> do
        UI.containerAdd desktopUI widget
        updateVisible deskUI -- This should show the window widget as well
      False -> do
        UI.widgetHide widget
        UI.containerRemove desktopUI widget
        updateVisible deskUI

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
  WindowSetup ->
  (Int -> Int -> DeskWindowUI -> IO ()) ->
  (Window -> IO ()) ->
  Task DWindowChange ->
  Task (Maybe Window) ->
  m UI.Widget
dVisWindowCont winSetup switcher reqActivate taskWinChange taskActive = do
  placeholder <- UI.imageNew
  UI.onWidgetRealize placeholder $ do
    winsRef <- newIORef M.empty
    activeRef <- newIORef Nothing
    killUpd <- UI.uiTask taskWinChange (structureUpd winsRef)
    killAct <- UI.uiTask taskActive (changeActivate winsRef activeRef)
    () <$ UI.onWidgetUnrealize placeholder (killAct >> killUpd)

  UI.toWidget placeholder
  where
    structureUpd winsRef = \case
      DWindowChange _newOrder addWins removed -> do
        -- Removes window
        for_ (S.toList removed) $ \window -> do
          mayUI <- atomicModifyIORef' winsRef $ swap . M.updateLookupWithKey (\_ _ -> Nothing) window
          for_ mayUI $ \DeskWindowUI{windowUI} -> do
            UI.widgetDestroy windowUI

        -- Adds window
        for_ (M.toList addWins) $ \(window, winTask) -> do
          existed <- (window `M.member`) <$> readIORef winsRef
          unless existed $ do
            winUI <- dVisWindowItem winSetup switcher reqActivate window winTask
            liftIO $ infoM "DeskVis" $ "UI: Added Window " <> show window
            modifyIORef' winsRef (M.insert window winUI)

    changeActivate winsRef activeRef nextActive = do
      windows <- readIORef winsRef
      prevActive <- atomicModifyIORef' activeRef (nextActive,)
      -- Little hack(?)
      for_ (prevActive >>= (windows M.!?)) $ \DeskWindowUI{windowUI} -> do
        UI.widgetGetStyleContext windowUI >>= UI.updateCssClass winActiveCssClass []
      for_ (nextActive >>= (windows M.!?)) $ \DeskWindowUI{windowUI} -> do
        UI.widgetGetStyleContext windowUI >>= UI.updateCssClass winActiveCssClass [()]

dVisWindowItem ::
  MonadIO m =>
  WindowSetup ->
  (Int -> Int -> DeskWindowUI -> IO ()) ->
  (Window -> IO ()) ->
  Window ->
  Task WindowInfo ->
  m DeskWindowUI
dVisWindowItem WindowSetup{..} switcher reqActivate window infoTask = do
  winImage <- UI.imageNew
  UI.widgetGetStyleContext winImage >>= flip UI.styleContextAddClass (T.pack "window-item")

  winImg <- UI.toWidget winImage
  winBtn <- UI.buttonNewWith (Just winImg) $ reqActivate window
  let dWinUI = DeskWindowUI window winBtn
  -- FIXME Takes too long

  -- Cannot rely on realization, since window widgets would be floating
  liftIO $ do
    curDesk <- newIORef @Int (-1) -- When desktop is -1, it should not be showed.
    kill <- UI.uiTask infoTask (updateWindow dWinUI winImage curDesk)
    UI.onWidgetDestroy winImage kill

  pure $ DeskWindowUI window winBtn
  where
    updateWindow dWinUI@DeskWindowUI{windowUI} winImage curDesk winInfo@WindowInfo{..} = do
      let newDesk = windowDesktop
      -- Switches the desktop
      oldDesk <- atomicModifyIORef' curDesk $ (newDesk,)
      when (newDesk /= oldDesk) $ switcher oldDesk newDesk dWinUI

      -- Update the icon
      let size = fromIntegral $ fromEnum UI.IconSizeLargeToolbar
      windowImgSetter winInfo >>= \case
        -- MAYBE use pixel size to reset.
        ImgSName name -> UI.imageSetFromIconName winImage (Just name) size
        ImgSGIcon icon -> UI.imageSetFromGicon winImage icon size

      -- Update the class
      UI.widgetGetStyleContext windowUI >>= UI.updateCssClass windowCssClass (S.toList windowState)

{-------------------------------------------------------------------
                          Communication
--------------------------------------------------------------------}

-- | Window changes, with new order, added & removed
data DWindowChange = DWindowChange (M.Map Window Int) (M.Map Window (Task WindowInfo)) (S.Set Window)

-- data DVWindowRcv = DWindowMap Window (Task WindowInfo) | DWindowUnmap Window

data DeskVisRcvs = DeskVisRcvs
  { desktopStats :: Task (V.Vector DesktopStat)
  , windowsChange :: Task DWindowChange
  , windowActive :: Task (Maybe Window)
  , reqActivate :: Window -> IO ()
  , reqToDesktop :: Int -> IO ()
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
    watchXQuery rootWin getAllWindows $ \newWindowsVec -> do
      let order = M.fromList $ zip (V.toList newWindowsVec) [0 ..]
      let newWindows = S.fromList $ V.toList newWindowsVec
      oldWindows <- liftIO $ atomicModifyIORef' windowRef $ \oldWindows -> (newWindows, oldWindows)
      let removed = oldWindows S.\\ newWindows
      let added = newWindows S.\\ oldWindows
      -- "Invalid" windows are ignored from here
      addWins <- M.traverseMaybeWithKey (\_ -> lift . watchWindow) (M.fromSet id added)
      pure $ DWindowChange order addWins removed

  windowActive <- errorAct $ watchXQuery rootWin getActiveWindow pure
  desktopStats <- errorAct $ watchXQuery rootWin getDesktopStat pure
  reqActivate <- reqActiveWindow True
  reqToDesktop <- reqCurrentDesktop

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
