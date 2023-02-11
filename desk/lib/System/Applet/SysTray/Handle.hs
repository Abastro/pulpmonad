module System.Applet.SysTray.Handle (
  SysTrayArgs (..),
  systemTray,
) where

import Control.Monad
import DBus
import DBus.Client
import Data.Foldable
import Data.GI.Base.Constructible qualified as Glib
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Traversable
import GI.DbusmenuGtk3.Objects.Menu qualified as DMenu
import Gtk.Commons qualified as Gtk
import Gtk.Task qualified as Gtk
import StatusNotifier.Host.Service qualified as HS
import StatusNotifier.Item.Client qualified as IC
import System.Applet.SysTray.SystemTrayView qualified as MainView
import System.Applet.SysTray.TrayItemView qualified as ItemView
import System.Posix.Process (getProcessID)

data SysTrayArgs = SysTrayArgs
  { trayOrientation :: !Gtk.Orientation
  , trayAlignBegin :: !Bool
  -- ^ Whether to align from the beginning
  }

-- | Starts system tray and presents it as widget.
-- Has undefined behavior if there are more than one such instance for a process.
--
-- Throws if the host cannot be started.
systemTray :: SysTrayArgs -> IO Gtk.Widget
systemTray SysTrayArgs{..} = do
  -- Creates its own dbus client.
  client <- connectSession
  procID <- getProcessID
  host <-
    maybe (fail "Cannot create host") pure
      =<< HS.build
        HS.defaultParams
          { HS.dbusClient = Just client
          , HS.uniqueIdentifier = "pulp-system-tray-" <> show procID
          }
  trayView <- Glib.new MainView.AsView [] -- trayOrientation trayAlignBegin
  MainView.setOrientation trayView trayOrientation
  MainView.setPackAt trayView (if trayAlignBegin then MainView.PackStart else MainView.PackEnd)

  SysTrayHandle <- sysTrayMake host client trayView
  Gtk.toWidget trayView

-- Currently, no way provided to externally handle system ray
data SysTrayHandle = SysTrayHandle

sysTrayMake :: HS.Host -> Client -> MainView.View -> IO SysTrayHandle
sysTrayMake HS.Host{..} client view = do
  registers =<< newIORef M.empty
  where
    registers itemsRef = do
      trayHandle <- addUpdateHandler $ \typ info -> Gtk.uiSingleRun (onUpdate info typ)
      Gtk.onWidgetDestroy view $ removeUpdateHandler trayHandle
      pure SysTrayHandle
      where
        onUpdate :: HS.ItemInfo -> HS.UpdateType -> IO ()
        onUpdate info@HS.ItemInfo{itemServiceName} typ = do
          items <- readIORef itemsRef
          items' <- M.alterF (alterItem info typ) itemServiceName items
          writeIORef itemsRef items'

        alterItem :: HS.ItemInfo -> HS.UpdateType -> Maybe TrayItemHandle -> IO (Maybe TrayItemHandle)
        alterItem info@HS.ItemInfo{..} = \case
          HS.ItemAdded -> \case
            Just item -> Just item <$ putStrLn "TODO, warn user: item exist"
            Nothing -> addItem info
          HS.ItemRemoved -> \case
            Nothing -> Nothing <$ putStrLn "TODO, warn user: item not exist"
            Just TrayItemHandle{itemAddRemove} -> do
              itemAddRemove view False
              pure Nothing

          -- Icon updates
          HS.IconUpdated -> maybe (addItem info) $ \item@TrayItemHandle{itemUpdateIcon} ->
            Just item <$ itemUpdateIcon iconThemePath (T.pack iconName) iconPixmaps
          HS.OverlayIconUpdated -> maybe (addItem info) $ \item@TrayItemHandle{itemUpdateOverlay} ->
            Just item <$ itemUpdateOverlay iconThemePath (T.pack <$> overlayIconName) overlayIconPixmaps
          -- Tooltip updates
          HS.ToolTipUpdated -> traverse $ \item@TrayItemHandle{itemUpdateTooltip} ->
            item <$ itemUpdateTooltip itemToolTip
          -- Not handled
          HS.StatusUpdated -> pure
          HS.TitleUpdated -> pure

        addItem :: HS.ItemInfo -> IO (Maybe TrayItemHandle)
        addItem info@HS.ItemInfo{..} = do
          itemView <- Glib.new ItemView.AsView []
          item@TrayItemHandle{..} <- trayItemMake client info itemView
          itemAddRemove view True
          itemUpdateIcon iconThemePath (T.pack iconName) iconPixmaps
          itemUpdateTooltip itemToolTip
          pure (Just item)

data TrayItemHandle = TrayItemHandle
  { itemAddRemove :: MainView.View -> Bool -> IO ()
  -- ^ True for add
  , itemUpdateIcon :: Maybe String -> T.Text -> HS.ImageInfo -> IO ()
  , itemUpdateOverlay :: Maybe String -> Maybe T.Text -> HS.ImageInfo -> IO ()
  , itemUpdateTooltip :: Maybe (String, HS.ImageInfo, String, String) -> IO ()
  }

-- MAYBE Status effect
trayItemMake :: Client -> HS.ItemInfo -> ItemView.View -> IO TrayItemHandle
trayItemMake client HS.ItemInfo{..} view = do
  mayMenu <- for menuPath $ \mPath ->
    DMenu.menuNew (T.pack . formatBusName $ itemServiceName) (T.pack . formatObjectPath $ mPath)
  ItemView.setInputHandler view $ \case
    ItemView.TrayItemScroll dir -> onScroll dir
    ItemView.TrayItemClick btn xRoot yRoot -> onClick mayMenu xRoot yRoot btn

  pure TrayItemHandle{..}
  where
    itemAddRemove trayView = \case
      True -> MainView.addItem trayView view
      False -> MainView.removeItem trayView view

    itemUpdateIcon itemThemePath iconName itemIconInfo = do
      ItemView.setIcon view ItemView.TrayItemIcon{itemIconName = Just iconName, ..}

    itemUpdateOverlay itemThemePath itemIconName itemIconInfo = do
      ItemView.setIcon view ItemView.TrayItemIcon{..}

    itemUpdateTooltip = \case
      Nothing -> Gtk.widgetSetTooltipText view Nothing
      Just (_, _, title, full) -> do
        Gtk.widgetSetTooltipText view (Just . T.pack $ tooltipOf (title, full))
      where
        tooltipOf = \case
          ("", f) -> f
          (t, "") -> t
          (t, f) -> t <> ": " <> f

    onScroll = \case
      ItemView.ScrollUp -> void $ IC.scroll client itemServiceName itemServicePath (-1) "vertical"
      ItemView.ScrollDown -> void $ IC.scroll client itemServiceName itemServicePath 1 "vertical"
      ItemView.ScrollLeft -> void $ IC.scroll client itemServiceName itemServicePath (-1) "horizontal"
      ItemView.ScrollRight -> void $ IC.scroll client itemServiceName itemServicePath 1 "horizontal"

    onClick mayMenu xRoot yRoot = \case
      ItemView.MouseLeft | not itemIsMenu -> void $ IC.activate client itemServiceName itemServicePath xRoot yRoot
      ItemView.MouseMiddle -> void $ IC.secondaryActivate client itemServiceName itemServicePath xRoot yRoot
      _ -> traverse_ (ItemView.showPopup view) mayMenu
