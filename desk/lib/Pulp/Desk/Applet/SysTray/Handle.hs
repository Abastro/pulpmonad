module Pulp.Desk.Applet.SysTray.Handle (
  SysTrayArgs (..),
  systemTray,
) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Unlift
import DBus qualified
import DBus.Client qualified as DBus
import Data.Foldable
import Data.GI.Base.Constructible qualified as GI
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Pulp.Desk.Applet.SysTray.SystemTrayView qualified as MainView
import Pulp.Desk.Applet.SysTray.TrayItemView qualified as ItemView
import Pulp.Desk.Env.PulpEnv
import Pulp.Desk.Reactive.Entry
import Pulp.Desk.Reactive.State
import Pulp.Desk.UI.Commons qualified as Gtk
import Pulp.Desk.UI.Reactive qualified as Gtk
import Pulp.Desk.Utils.LogPrint
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import StatusNotifier.Host.Service qualified as Host
import StatusNotifier.Item.Client qualified as SNItem
import System.Posix.Process (getProcessID)

data SysTrayArgs = SysTrayArgs
  { trayOrientation :: !Gtk.Orientation
  , trayAlignBegin :: !Bool
  -- ^ Whether to align from the beginning
  }

logName = T.pack "SysTray"

-- | Starts system tray and presents it as widget.
--
-- Throws if the host cannot be started.
systemTray :: SysTrayArgs -> PulpIO Gtk.Widget
systemTray SysTrayArgs{..} = withRunInIO $ \unlift -> do
  client <- DBus.connectSession
  procID <- getProcessID
  host <-
    maybe (fail "Cannot create host for system tray") pure
      =<< Host.build
        Host.defaultParams
          { Host.dbusClient = Just client
          , Host.uniqueIdentifier = "PulpSystemTray" <> show procID
          }
  trayView <- GI.new MainView.AsView []
  MainView.setOrientation trayView trayOrientation
  MainView.setPackAt trayView (if trayAlignBegin then MainView.PackStart else MainView.PackEnd)

  actuated <- newEmptyMVar
  network <- compile $ do
    -- We have no reason to unregister this one.
    -- NOTE: ItemAdded event seems to invoke for existing items on host register.
    eAllUpdates <- sourceEvent (sourceWaitTill actuated $ sniSource host)
    let eUpdate = filterJust (uncurry splitUpdate <$> eAllUpdates)
        (eColChange, eNormalUp) = split eUpdate
    (_, bItems) <- exeAccumD M.empty (modifyItems unlift client eNormalUp <$> eColChange)

    -- Use changes to apply to MainView collection.
    eItemDiffs <- diffEvent (-->) (AsCacheMap . M.map (\item -> item.view) <$> bItems)
    reactimate' $ fmap @Future (applyItemDiffs trayView) <$> eItemDiffs

  actuate network
  putMVar actuated ()

  Gtk.toWidget trayView

sniSource :: Host.Host -> Source (Host.UpdateType, Host.ItemInfo)
sniSource Host.Host{..} = sourceWithUnreg $ \handler -> do
  handlerId <- addUpdateHandler (curry handler)
  pure $ removeUpdateHandler handlerId

applyItemDiffs :: MainView.View -> PatchCol ItemView.View -> IO ()
applyItemDiffs main = applyImpure $ \case
  Insert item -> MainView.addItem main item
  Delete item -> MainView.removeItem main item

data NormalUpdateType = IconUpdate | OverlayIconUpdate | TooltipUpdate
  deriving (Show)
data NormalUpdate = NormalUpdateOf !NormalUpdateType !Host.ItemInfo

-- Use ColOp because it exists
splitUpdate :: Host.UpdateType -> Host.ItemInfo -> Maybe (Either (ColOp Host.ItemInfo) NormalUpdate)
splitUpdate = \case
  Host.ItemAdded -> Just . Left . Insert
  Host.ItemRemoved -> Just . Left . Delete
  Host.IconUpdated -> Just . Right . NormalUpdateOf IconUpdate
  Host.OverlayIconUpdated -> Just . Right . NormalUpdateOf OverlayIconUpdate
  Host.ToolTipUpdated -> Just . Right . NormalUpdateOf TooltipUpdate
  _ -> const Nothing

data TrayItem = MkTrayItem
  { view :: ItemView.View
  , delete :: IO ()
  }

modifyItems ::
  (forall a. PulpIO a -> IO a) ->
  DBus.Client ->
  Event NormalUpdate ->
  ColOp Host.ItemInfo ->
  M.Map DBus.BusName TrayItem ->
  MomentIO (M.Map DBus.BusName TrayItem)
modifyItems unlift client eNormalUp colOp = M.alterF after serviceName
  where
    after old = case (colOp, old) of
      (Insert newItem, Nothing) -> do
        liftIO . unlift $
          logS logName LevelDebug (logStrf "Inserting item ($1)" $ show serviceName)
        -- Filters the update event
        new <- createTrayItem client (filterE isThisUpdate eNormalUp) newItem
        pure (Just new)
      (Delete _, Just old) -> do
        liftIO . unlift $
          logS logName LevelDebug (logStrf "Deleting item ($1)" $ show serviceName)
        liftIO old.delete
        pure Nothing
      (Insert _, old) ->
        old <$ do
          liftIO . unlift $
            logS logName LevelWarn (logStrf "Tried to insert existing item ($1)" $ show serviceName)
      (Delete _, old) ->
        old <$ do
          liftIO . unlift $
            logS logName LevelWarn (logStrf "Tried to delete nonexistent item ($1)" $ show serviceName)

    isThisUpdate (NormalUpdateOf _ updItem) = serviceName == updItem.itemServiceName

    serviceName = case colOp of
      Insert Host.ItemInfo{itemServiceName} -> itemServiceName
      Delete Host.ItemInfo{itemServiceName} -> itemServiceName

createTrayItem :: DBus.Client -> Event NormalUpdate -> Host.ItemInfo -> MomentIO TrayItem
createTrayItem client eNormalUp info@Host.ItemInfo{..} = do
  -- Needs the view right away.
  view <- liftIO $ takeMVar =<< Gtk.uiCreate (GI.new ItemView.AsView [])

  for_ menuPath $ \path -> liftIO $ do
    ItemView.setMenu view (T.pack . DBus.formatBusName $ itemServiceName, T.pack . DBus.formatObjectPath $ path)

  (eClick, kill1) <- sourceEventWA (ItemView.clickSource view)
  (eScroll, kill2) <- sourceEventWA (ItemView.scrollSource view)

  -- TODO Make these a Behavior, because they theoretically are.
  -- There is no syncBehavior for this yet.
  liftIO $ ItemView.setIcon view (iconOf info)
  kill3 <- reactEvent $ ItemView.setIcon view <$> filterJust (iconPart <$> eNormalUp)

  liftIO $ ItemView.setOverlay view (overlayOf info)
  kill4 <- reactEvent $ ItemView.setOverlay view <$> filterJust (overlayPart <$> eNormalUp)

  liftIO $ updateTooltip view itemToolTip
  kill5 <- reactEvent $ updateTooltip view <$> filterJust (tooltipPart <$> eNormalUp)

  -- Reacts to the events here as well.
  kill6 <- reactEvent $ handleClick client info view <$> eClick
  kill7 <- reactEvent $ handleScroll client info <$> eScroll

  let delete = kill1 <> kill2 <> kill3 <> kill4 <> kill5 <> kill6 <> kill7
  pure MkTrayItem{..}
  where
    iconPart = \case
      NormalUpdateOf IconUpdate item -> Just (iconOf item)
      _ -> Nothing

    overlayPart = \case
      NormalUpdateOf OverlayIconUpdate item -> Just (overlayOf item)
      _ -> Nothing

    tooltipPart = \case
      NormalUpdateOf TooltipUpdate Host.ItemInfo{..} -> Just itemToolTip
      _ -> Nothing

iconOf :: Host.ItemInfo -> ItemView.TrayItemIcon
iconOf Host.ItemInfo{..} =
  ItemView.TrayItemIcon
    { itemThemePath = iconThemePath
    , itemIconName = Just $ T.pack iconName
    , itemIconInfo = iconPixmaps
    }

overlayOf :: Host.ItemInfo -> ItemView.TrayItemIcon
overlayOf Host.ItemInfo{..} =
  ItemView.TrayItemIcon
    { itemThemePath = iconThemePath
    , itemIconName = T.pack <$> overlayIconName
    , itemIconInfo = overlayIconPixmaps
    }

updateTooltip :: ItemView.View -> Maybe (String, Host.ImageInfo, String, String) -> IO ()
updateTooltip view = \case
  Nothing -> ItemView.setTooltip view Nothing
  Just (_, _, title, full) -> ItemView.setTooltip view (Just . T.pack $ tooltipOf (title, full))
  where
    tooltipOf = \case
      ("", f) -> f
      (t, "") -> t
      (t, f) -> t <> ": " <> f

handleClick :: DBus.Client -> Host.ItemInfo -> ItemView.View -> ItemView.MouseClick -> IO ()
handleClick client Host.ItemInfo{..} view (ItemView.MouseClickOf event xRoot yRoot mouse) = case mouse of
  ItemView.MouseLeft | not itemIsMenu -> void $ SNItem.activate client itemServiceName itemServicePath xRoot yRoot
  ItemView.MouseMiddle -> void $ SNItem.secondaryActivate client itemServiceName itemServicePath xRoot yRoot
  _ -> ItemView.showPopup view event

handleScroll :: DBus.Client -> Host.ItemInfo -> ItemView.ScrollDir -> IO ()
handleScroll client Host.ItemInfo{..} = \case
  ItemView.ScrollUp -> scrollOf (-1) "vertical"
  ItemView.ScrollDown -> scrollOf 1 "vertical"
  ItemView.ScrollLeft -> scrollOf (-1) "horizontal"
  ItemView.ScrollRight -> scrollOf 1 "horizontal"
  where
    scrollOf move dir = void $ SNItem.scroll client itemServiceName itemServicePath move dir
