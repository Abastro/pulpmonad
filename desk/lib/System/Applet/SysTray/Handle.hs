module System.Applet.SysTray.Handle (
  SysTrayArgs (..),
  systemTray,
) where

import Control.Concurrent.MVar
import Control.Event.Entry
import Control.Event.State
import Control.Monad
import DBus
import DBus.Client
import Data.Foldable
import Data.GI.Base.Constructible
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Traversable
import GI.DbusmenuGtk3.Objects.Menu qualified as DMenu
import GI.Gtk.Objects.Menu qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Task qualified as Gtk
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import StatusNotifier.Host.Service qualified as HS
import StatusNotifier.Item.Client qualified as IC
import System.Applet.SysTray.SystemTrayView qualified as MainView
import System.Applet.SysTray.TrayItemView qualified as ItemView
import System.IO
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
  client <- connectSession
  procID <- getProcessID
  host <-
    maybe (fail "Cannot create host") pure
      =<< HS.build
        HS.defaultParams
          { HS.dbusClient = Just client
          , HS.uniqueIdentifier = "pulp-system-tray-" <> show procID
          }
  trayView <- new MainView.AsView []
  MainView.setOrientation trayView trayOrientation
  MainView.setPackAt trayView (if trayAlignBegin then MainView.PackStart else MainView.PackEnd)

  actuated <- newEmptyMVar
  network <- compile $ do
    -- We have no reason to unregister this one.
    -- NOTE: ItemAdded event seems to invoke for existing items on host register.
    eAllUpdates <- sourceEvent (sniSource host actuated)
    let eUpdate = filterJust (uncurry splitUpdate <$> eAllUpdates)
        (eColChange, eNormalUp) = split eUpdate
    -- Discards results for now.
    (eCh, _) <- exeMapAccum M.empty (fmap (fmap ((),)) . modifyItems client trayView eNormalUp <$> eColChange)
    -- TODO This is a convenient place to insert a log.
    reactimate (hPutStrLn stderr "[System Tray] Item Added or Removed" <$ eCh)
    pure ()

  actuate network
  putMVar actuated ()

  Gtk.toWidget trayView

sniSource :: HS.Host -> MVar () -> Source (HS.UpdateType, HS.ItemInfo)
sniSource HS.Host{..} wait = sourceWithUnreg $ \handler -> do
  handlerId <- addUpdateHandler $ \typ info -> do
    () <- readMVar wait -- Waits until network could handle. Takes some overhead every loop, but eh.
    handler (typ, info)
  pure $ removeUpdateHandler handlerId

data NormalUpdateType = IconUpdate | OverlayIconUpdate | TooltipUpdate
  deriving (Show)
data NormalUpdate = NormalUpdateOf !NormalUpdateType !HS.ItemInfo

-- Use ColOp because it exists
splitUpdate :: HS.UpdateType -> HS.ItemInfo -> Maybe (Either (ColOp HS.ItemInfo) NormalUpdate)
splitUpdate = \case
  HS.ItemAdded -> Just . Left . Insert
  HS.ItemRemoved -> Just . Left . Delete
  HS.IconUpdated -> Just . Right . NormalUpdateOf IconUpdate
  HS.OverlayIconUpdated -> Just . Right . NormalUpdateOf OverlayIconUpdate
  HS.ToolTipUpdated -> Just . Right . NormalUpdateOf TooltipUpdate
  _ -> const Nothing

newtype TrayItem = MkTrayItem {deleteTrayItem :: IO ()}

modifyItems ::
  Client ->
  MainView.View ->
  Event NormalUpdate ->
  ColOp HS.ItemInfo ->
  M.Map BusName TrayItem ->
  MomentIO (M.Map BusName TrayItem)
modifyItems client mainView eNormalUp colOp = \m -> do
  M.alterF after serviceName m
  where
    after old = case (colOp, old) of
      -- Will update view here as well
      (Insert newItem, Nothing) -> do
        -- Filters the update event
        new <- createTrayItem client newItem mainView (filterE isThisUpdate eNormalUp)
        pure (Just new)
      (Delete _, Just old) -> do
        liftIO (deleteTrayItem old)
        pure Nothing
      -- Error cases - TODO: Warn user properly
      (Insert _, old) -> old <$ liftIO (hPutStrLn stderr "Tried to insert when item already exist")
      (Delete _, old) -> old <$ liftIO (hPutStrLn stderr "Tried to delete when item did not exist")

    isThisUpdate (NormalUpdateOf _ HS.ItemInfo{itemServiceName}) = serviceName == itemServiceName

    serviceName = case colOp of
      Insert HS.ItemInfo{itemServiceName} -> itemServiceName
      Delete HS.ItemInfo{itemServiceName} -> itemServiceName

createTrayItem :: Client -> HS.ItemInfo -> MainView.View -> Event NormalUpdate -> MomentIO TrayItem
createTrayItem client info@HS.ItemInfo{..} mainView eNormalUp = do
  -- Needs the view right away.
  itemView <- liftIO $ takeMVar =<< Gtk.uiCreate (new ItemView.AsView [])

  menuVar <- liftIO . Gtk.uiCreate $ for menuPath $ \mPath ->
    DMenu.menuNew (T.pack . formatBusName $ itemServiceName) (T.pack . formatObjectPath $ mPath)

  (eClick, kill1) <- sourceEventWA (ItemView.clickSource itemView)
  (eScroll, kill2) <- sourceEventWA (ItemView.scrollSource itemView)

  -- Run updates; Not putting in Behavior now, as it could incur memory cost.
  liftIO $ ItemView.setIcon itemView (iconOf info)
  kill3 <- reactEvent $ ItemView.setIcon itemView <$> filterJust (iconPart <$> eNormalUp)

  liftIO $ ItemView.setOverlay itemView (overlayOf info)
  kill4 <- reactEvent $ ItemView.setOverlay itemView <$> filterJust (overlayPart <$> eNormalUp)

  liftIO $ updateTooltip itemView itemToolTip
  kill5 <- reactEvent $ updateTooltip itemView <$> filterJust (tooltipPart <$> eNormalUp)

  -- Reacts to the events here as well.
  mayMenu <- liftIO $ takeMVar menuVar
  kill6 <- reactEvent $ handleClick client info mayMenu itemView <$> eClick
  kill7 <- reactEvent $ handleScroll client info <$> eScroll

  -- Main view operations.
  let addItem = MainView.addItem mainView itemView
      removeItem = MainView.removeItem mainView itemView

  liftIO addItem
  let deleteTrayItem = kill1 <> kill2 <> kill3 <> kill4 <> kill5 <> kill6 <> kill7 <> removeItem
  pure MkTrayItem{..}
  where
    iconPart = \case
      NormalUpdateOf IconUpdate item -> Just (iconOf item)
      _ -> Nothing

    overlayPart = \case
      NormalUpdateOf OverlayIconUpdate item -> Just (overlayOf item)
      _ -> Nothing

    tooltipPart = \case
      NormalUpdateOf TooltipUpdate HS.ItemInfo{..} -> Just itemToolTip
      _ -> Nothing

iconOf :: HS.ItemInfo -> ItemView.TrayItemIcon
iconOf HS.ItemInfo{..} =
  ItemView.TrayItemIcon
    { itemThemePath = iconThemePath
    , itemIconName = Just $ T.pack iconName
    , itemIconInfo = iconPixmaps
    }

overlayOf :: HS.ItemInfo -> ItemView.TrayItemIcon
overlayOf HS.ItemInfo{..} =
  ItemView.TrayItemIcon
    { itemThemePath = iconThemePath
    , itemIconName = T.pack <$> overlayIconName
    , itemIconInfo = overlayIconPixmaps
    }

updateTooltip :: ItemView.View -> Maybe (String, HS.ImageInfo, String, String) -> IO ()
updateTooltip view = \case
  Nothing -> ItemView.setTooltip view Nothing
  Just (_, _, title, full) -> ItemView.setTooltip view (Just . T.pack $ tooltipOf (title, full))
  where
    tooltipOf = \case
      ("", f) -> f
      (t, "") -> t
      (t, f) -> t <> ": " <> f

handleClick :: Client -> HS.ItemInfo -> Maybe DMenu.Menu -> ItemView.View -> ItemView.MouseClick -> IO ()
handleClick client HS.ItemInfo{..} mayMenu view (ItemView.MouseClickOf xRoot yRoot mouse) = case mouse of
  ItemView.MouseLeft | not itemIsMenu -> void $ IC.activate client itemServiceName itemServicePath xRoot yRoot
  ItemView.MouseMiddle -> void $ IC.secondaryActivate client itemServiceName itemServicePath xRoot yRoot
  _ -> traverse_ (popupAt view) mayMenu
  where
    -- Popup could only be opened with GTK events, this is a workaround for this.
    popupAt view menu = Gtk.menuPopupAtWidget menu view Gtk.GravitySouthWest Gtk.GravityNorthWest Nothing

handleScroll :: Client -> HS.ItemInfo -> ItemView.ScrollDir -> IO ()
handleScroll client HS.ItemInfo{..} = \case
  ItemView.ScrollUp -> scrollOf (-1) "vertical"
  ItemView.ScrollDown -> scrollOf 1 "vertical"
  ItemView.ScrollLeft -> scrollOf (-1) "horizontal"
  ItemView.ScrollRight -> scrollOf 1 "horizontal"
  where
    scrollOf move dir = void $ IC.scroll client itemServiceName itemServicePath move dir
