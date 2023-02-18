module System.Applet.SysTray.Handle (
  SysTrayArgs (..),
  systemTray,
) where

import Control.Concurrent.MVar
import Control.Event.Entry
import Control.Event.State
import Control.Monad
import Control.Monad.IO.Unlift
import DBus
import DBus.Client
import Data.Foldable
import Data.GI.Base.Constructible
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Gtk.Commons qualified as Gtk
import Gtk.Task qualified as Gtk
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import StatusNotifier.Host.Service qualified as HS
import StatusNotifier.Item.Client qualified as IC
import System.Applet.SysTray.SystemTrayView qualified as MainView
import System.Applet.SysTray.TrayItemView qualified as ItemView
import System.Log.LogPrint
import System.Posix.Process (getProcessID)
import System.Pulp.PulpEnv

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
  client <- connectSession
  procID <- getProcessID
  host <-
    maybe (fail "Cannot create host for system tray") pure
      =<< HS.build
        HS.defaultParams
          { HS.dbusClient = Just client
          , HS.uniqueIdentifier = "PulpSystemTray" <> show procID
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
    (_, bItems) <- exeAccumD M.empty (modifyItems unlift client eNormalUp <$> eColChange)

    -- Use changes to apply to MainView collection.
    eItemDiffs <- diffEvent (-->) (AsCacheMap . M.map itemView <$> bItems)
    reactimate' $ fmap @Future (applyItemDiffs trayView) <$> eItemDiffs

  actuate network
  putMVar actuated ()

  Gtk.toWidget trayView

sniSource :: HS.Host -> MVar () -> Source (HS.UpdateType, HS.ItemInfo)
sniSource HS.Host{..} wait = sourceWithUnreg $ \handler -> do
  handlerId <- addUpdateHandler $ \typ info -> do
    () <- readMVar wait -- Waits until network could handle. Takes some overhead every loop, but eh.
    handler (typ, info)
  pure $ removeUpdateHandler handlerId

applyItemDiffs :: MainView.View -> PatchCol ItemView.View -> IO ()
applyItemDiffs main = applyImpure $ \case
  Insert item -> MainView.addItem main item
  Delete item -> MainView.removeItem main item

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

data TrayItem = MkTrayItem
  { itemView :: ItemView.View
  , deleteTrayItem :: IO ()
  }

modifyItems ::
  (forall a. PulpIO a -> IO a) ->
  Client ->
  Event NormalUpdate ->
  ColOp HS.ItemInfo ->
  M.Map BusName TrayItem ->
  MomentIO (M.Map BusName TrayItem)
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
        liftIO (deleteTrayItem old)
        pure Nothing
      (Insert _, old) -> old <$ do
        liftIO . unlift $
          logS logName LevelWarn (logStrf "Tried to insert existing item ($1)" $ show serviceName)
      (Delete _, old) -> old <$ do
        liftIO . unlift $
          logS logName LevelWarn (logStrf "Tried to delete nonexistent item ($1)" $ show serviceName)

    isThisUpdate (NormalUpdateOf _ HS.ItemInfo{itemServiceName}) = serviceName == itemServiceName

    serviceName = case colOp of
      Insert HS.ItemInfo{itemServiceName} -> itemServiceName
      Delete HS.ItemInfo{itemServiceName} -> itemServiceName

createTrayItem :: Client -> Event NormalUpdate -> HS.ItemInfo -> MomentIO TrayItem
createTrayItem client eNormalUp info@HS.ItemInfo{..} = do
  -- Needs the view right away.
  itemView <- liftIO $ takeMVar =<< Gtk.uiCreate (new ItemView.AsView [])

  for_ menuPath $ \path -> liftIO $ do
    ItemView.setMenu itemView (T.pack . formatBusName $ itemServiceName, T.pack . formatObjectPath $ path)

  (eClick, kill1) <- sourceEventWA (ItemView.clickSource itemView)
  (eScroll, kill2) <- sourceEventWA (ItemView.scrollSource itemView)

  -- TODO Make these a Behavior, because they theoretically are.
  -- There is no syncBehavior for this yet.
  liftIO $ ItemView.setIcon itemView (iconOf info)
  kill3 <- reactEvent $ ItemView.setIcon itemView <$> filterJust (iconPart <$> eNormalUp)

  liftIO $ ItemView.setOverlay itemView (overlayOf info)
  kill4 <- reactEvent $ ItemView.setOverlay itemView <$> filterJust (overlayPart <$> eNormalUp)

  liftIO $ updateTooltip itemView itemToolTip
  kill5 <- reactEvent $ updateTooltip itemView <$> filterJust (tooltipPart <$> eNormalUp)

  -- Reacts to the events here as well.
  kill6 <- reactEvent $ handleClick client info itemView <$> eClick
  kill7 <- reactEvent $ handleScroll client info <$> eScroll

  let deleteTrayItem = kill1 <> kill2 <> kill3 <> kill4 <> kill5 <> kill6 <> kill7
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

handleClick :: Client -> HS.ItemInfo -> ItemView.View -> ItemView.MouseClick -> IO ()
handleClick client HS.ItemInfo{..} view (ItemView.MouseClickOf event xRoot yRoot mouse) = case mouse of
  ItemView.MouseLeft | not itemIsMenu -> void $ IC.activate client itemServiceName itemServicePath xRoot yRoot
  ItemView.MouseMiddle -> void $ IC.secondaryActivate client itemServiceName itemServicePath xRoot yRoot
  _ -> ItemView.showPopup view event

handleScroll :: Client -> HS.ItemInfo -> ItemView.ScrollDir -> IO ()
handleScroll client HS.ItemInfo{..} = \case
  ItemView.ScrollUp -> scrollOf (-1) "vertical"
  ItemView.ScrollDown -> scrollOf 1 "vertical"
  ItemView.ScrollLeft -> scrollOf (-1) "horizontal"
  ItemView.ScrollRight -> scrollOf 1 "horizontal"
  where
    scrollOf move dir = void $ IC.scroll client itemServiceName itemServicePath move dir
