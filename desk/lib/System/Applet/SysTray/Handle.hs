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
import System.Applet.SysTray.SystemTrayView qualified as View
import System.Applet.SysTray.TrayItemView qualified as View
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
  trayView <- Glib.new View.SystemTrayView [] -- trayOrientation trayAlignBegin
  View.traySetOrientation trayView trayOrientation
  View.traySetPackAt trayView (if trayAlignBegin then View.PackStart else View.PackEnd)

  SysTrayHandle <- sysTrayMake host client trayView
  Gtk.toWidget trayView

-- Currently, no way provided to externally handle system ray
data SysTrayHandle = SysTrayHandle

sysTrayMake :: HS.Host -> Client -> View.SystemTrayView -> IO SysTrayHandle
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
          itemView <- Glib.new View.TrayItemView []
          item@TrayItemHandle{..} <- trayItemMake client info itemView
          itemAddRemove view True
          itemUpdateIcon iconThemePath (T.pack iconName) iconPixmaps
          itemUpdateTooltip itemToolTip
          pure (Just item)

data TrayItemHandle = TrayItemHandle
  { itemAddRemove :: View.SystemTrayView -> Bool -> IO ()
  -- ^ True for add
  , itemUpdateIcon :: Maybe String -> T.Text -> HS.ImageInfo -> IO ()
  , itemUpdateOverlay :: Maybe String -> Maybe T.Text -> HS.ImageInfo -> IO ()
  , itemUpdateTooltip :: Maybe (String, HS.ImageInfo, String, String) -> IO ()
  }

-- MAYBE Status effect
trayItemMake :: Client -> HS.ItemInfo -> View.TrayItemView -> IO TrayItemHandle
trayItemMake client HS.ItemInfo{..} view = do
  mayMenu <- for menuPath $ \mPath ->
    DMenu.menuNew (T.pack . formatBusName $ itemServiceName) (T.pack . formatObjectPath $ mPath)
  View.itemSetInputHandler view $ \case
    View.TrayItemScroll dir -> onScroll dir
    View.TrayItemClick btn xRoot yRoot -> onClick mayMenu xRoot yRoot btn

  pure TrayItemHandle{..}
  where
    itemAddRemove trayView = \case
      True -> View.trayAddItem trayView view
      False -> View.trayRemoveItem trayView view

    itemUpdateIcon itemThemePath iconName itemIconInfo = do
      View.itemSetIcon view View.TrayItemIcon{itemIconName = Just iconName, ..}

    itemUpdateOverlay itemThemePath itemIconName itemIconInfo = do
      View.itemSetIcon view View.TrayItemIcon{..}

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
      Gtk.ScrollDirectionUp -> void $ IC.scroll client itemServiceName itemServicePath (-1) "vertical"
      Gtk.ScrollDirectionDown -> void $ IC.scroll client itemServiceName itemServicePath 1 "vertical"
      Gtk.ScrollDirectionLeft -> void $ IC.scroll client itemServiceName itemServicePath (-1) "horizontal"
      Gtk.ScrollDirectionRight -> void $ IC.scroll client itemServiceName itemServicePath 1 "horizontal"
      _ -> pure ()

    onClick mayMenu xRoot yRoot = \case
      View.MouseLeft | not itemIsMenu -> void $ IC.activate client itemServiceName itemServicePath xRoot yRoot
      View.MouseMiddle -> void $ IC.secondaryActivate client itemServiceName itemServicePath xRoot yRoot
      _ -> traverse_ (View.itemShowPopup view) mayMenu
