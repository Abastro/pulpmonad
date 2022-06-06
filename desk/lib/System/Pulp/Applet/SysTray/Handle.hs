module System.Pulp.Applet.SysTray.Handle (
  SysTrayArgs (..),
  systemTray,
) where

import Control.Monad
import DBus
import DBus.Client
import Data.Foldable
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Traversable
import GI.DbusmenuGtk3.Objects.Menu qualified as DMenu
import StatusNotifier.Host.Service qualified as HS
import StatusNotifier.Item.Client qualified as IC
import System.Pulp.Applet.SysTray.View qualified as View
import UI.Commons qualified as UI
import UI.Task qualified as UI

data SysTrayArgs = SysTrayArgs
  { trayOrientation :: !UI.Orientation
  , trayIconSize :: !UI.IconSize
  , -- | Whether to align from the beginning
    trayAlignBegin :: !Bool
  }

-- | Starts system tray and presents it as widget.
-- Has undefined behavior if there are more than one such instance for entire setup.
--
-- Throws if the host cannot be started.
systemTray :: SysTrayArgs -> IO UI.Widget
systemTray args@SysTrayArgs{..} = do
  -- Creates its own dbus client.
  client <- connectSession
  host <-
    maybe (fail "Cannot create host") pure
      =<< HS.build
        HS.defaultParams
          { HS.dbusClient = Just client
          , HS.uniqueIdentifier = "pulp-system-tray"
          }
  trayView <- View.sysTrayNew trayOrientation trayAlignBegin
  _ <- sysTrayMake host client args trayView
  pure (View.sysTrayWidget trayView)

-- Currently, no way provided to externally handle system ray
data SysTrayHandle = SysTrayHandle

sysTrayMake :: HS.Host -> Client -> SysTrayArgs -> View.SysTray -> IO SysTrayHandle
sysTrayMake HS.Host{..} client SysTrayArgs{trayIconSize} view = do
  registers =<< newIORef M.empty
  where
    registers itemsRef = do
      trayHandle <- addUpdateHandler $ \typ info -> UI.uiSingleRun (onUpdate info typ)
      UI.onWidgetDestroy (View.sysTrayWidget view) $ removeUpdateHandler trayHandle
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
          itemView <- View.trayItemNew trayIconSize
          item@TrayItemHandle{..} <- trayItemMake client info itemView
          itemAddRemove view True
          itemUpdateIcon iconThemePath (T.pack iconName) iconPixmaps
          itemUpdateTooltip itemToolTip
          pure (Just item)

data TrayItemHandle = TrayItemHandle
  { -- | True for add
    itemAddRemove :: View.SysTray -> Bool -> IO ()
  , itemUpdateIcon :: Maybe String -> T.Text -> HS.ImageInfo -> IO ()
  , itemUpdateOverlay :: Maybe String -> Maybe T.Text -> HS.ImageInfo -> IO ()
  , itemUpdateTooltip :: Maybe (String, HS.ImageInfo, String, String) -> IO ()
  }

-- MAYBE Status effect
trayItemMake :: Client -> HS.ItemInfo -> View.TrayItem -> IO TrayItemHandle
trayItemMake client HS.ItemInfo{..} view = do
  mayMenu <- for menuPath $ \mPath ->
    DMenu.menuNew (T.pack . formatBusName $ itemServiceName) (T.pack . formatObjectPath $ mPath)
  View.trayItemCtrl view . View.ItemSetInputHandler $ \case
    View.TrayItemScroll dir -> onScroll dir
    View.TrayItemClick btn xRoot yRoot -> onClick mayMenu xRoot yRoot btn

  pure TrayItemHandle{..}
  where
    itemAddRemove trayView = \case
      True -> View.sysTrayCtrl trayView (View.TrayAddItem view)
      False -> View.sysTrayCtrl trayView (View.TrayRemoveItem view)

    itemUpdateIcon itemThemePath iconName itemIconInfo = do
      View.trayItemCtrl view (View.ItemSetIcon View.TrayItemIcon{..})
      where
        itemIconName = Just iconName

    itemUpdateOverlay itemThemePath itemIconName itemIconInfo = do
      View.trayItemCtrl view (View.ItemSetOverlay View.TrayItemIcon{..})

    itemUpdateTooltip = \case
      Nothing -> View.trayItemCtrl view (View.ItemSetTooltip Nothing)
      Just (_, _, title, full) -> do
        View.trayItemCtrl view $
          View.ItemSetTooltip . Just . T.pack $ case (title, full) of
            ("", f) -> f
            (t, "") -> t
            (t, f) -> t <> ": " <> f

    onScroll = \case
      UI.ScrollDirectionUp -> void $ IC.scroll client itemServiceName itemServicePath (-1) "vertical"
      UI.ScrollDirectionDown -> void $ IC.scroll client itemServiceName itemServicePath 1 "vertical"
      UI.ScrollDirectionLeft -> void $ IC.scroll client itemServiceName itemServicePath (-1) "horizontal"
      UI.ScrollDirectionRight -> void $ IC.scroll client itemServiceName itemServicePath 1 "horizontal"
      _ -> pure ()

    onClick mayMenu xRoot yRoot = \case
      View.MouseLeft | not itemIsMenu -> void $ IC.activate client itemServiceName itemServicePath xRoot yRoot
      View.MouseMiddle -> void $ IC.secondaryActivate client itemServiceName itemServicePath xRoot yRoot
      _ -> traverse_ (View.trayItemCtrl view . View.ItemShowPopup) mayMenu
