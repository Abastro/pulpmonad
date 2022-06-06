module System.Pulp.Applet.SysTray.Handle (
  SysTrayArgs (..),
  systemTray,
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import DBus
import DBus.Client
import Data.ByteString qualified as BS
import Data.Foldable
import Data.IORef
import Data.Int
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Traversable
import GI.DbusmenuGtk3.Objects.Menu qualified as DMenu
import GI.GLib.Structs.Bytes qualified as Glib
import GI.Gdk.Objects.Screen qualified as Gdk
import GI.GdkPixbuf.Enums qualified as Gdk
import GI.GdkPixbuf.Objects.Pixbuf qualified as Gdk
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gio.Objects.FileIcon qualified as Gio
import GI.Gtk.Flags qualified as UI
import GI.Gtk.Objects.IconTheme qualified as UI
import StatusNotifier.Host.Service qualified as HS
import StatusNotifier.Item.Client qualified as IC
import System.FilePath ((</>))
import System.Pulp.Applet.SysTray.View qualified as View
import UI.Commons qualified as UI
import UI.Task qualified as UI
import View.Imagery qualified as View
import qualified GI.Gio.Objects.ThemedIcon as Gio

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
sysTrayMake HS.Host{..} client args@SysTrayArgs{..} view = do
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

          -- Icon updates.
          -- NB: Why adding item when "icon update yet missing"?
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
          item@TrayItemHandle{..} <- trayItemMake client args info itemView
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

-- MAYBE Emphasis with status
trayItemMake :: Client -> SysTrayArgs -> HS.ItemInfo -> View.TrayItem -> IO TrayItemHandle
trayItemMake client SysTrayArgs{..} HS.ItemInfo{..} view = do
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

    itemUpdateIcon themePath iconName iconInfo = do
      iconSet <-
        runMaybeT $ imgNameSet trayIconSize themePath iconName <|> imgInfoSet trayIconSize iconInfo
      let defedSet = fromMaybe (View.ImgSName $ T.pack "missing") iconSet
      View.trayItemCtrl view (View.ItemSetIcon defedSet)

    -- TODO Handle this overlay setting
    itemUpdateOverlay _themePath _iconName _iconInfo = pure ()

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

-- TODO Need scaling somewhere

customIconTheme :: MonadIO m => String -> m UI.IconTheme
customIconTheme themePath = do
  custom <- UI.iconThemeNew
  Gdk.screenGetDefault >>= traverse_ (UI.iconThemeSetScreen custom)
  UI.iconThemeAppendSearchPath custom themePath

  defTheme <- UI.iconThemeGetDefault
  UI.iconThemeGetSearchPath defTheme >>= traverse_ (UI.iconThemeAppendSearchPath custom)
  pure custom

imgNameSet :: UI.IconSize -> Maybe String -> T.Text -> MaybeT IO View.ImageSet
imgNameSet size mayTheme name = do
  -- Icon should be freedesktop-compliant icon name.
  let nonEmp p = p <$ guard (not $ null p)
  case mayTheme >>= nonEmp of
    Nothing -> withDefTheme
    Just themePath ->
      forCustomTheme themePath <|> directPath (themePath </> T.unpack name)
  where
    panelName = name <> T.pack "-panel" -- Looks up first with "-panel" suffix
    loadFlags = [UI.IconLookupFlagsUseBuiltin, UI.IconLookupFlagsGenericFallback]
    withDefTheme = do
      -- Avoid rendering here. If possible, want to prioritize panel name.
      defTheme <- UI.iconThemeGetDefault
      themed <- UI.iconThemeHasIcon defTheme panelName >>= \case
        True -> Gio.themedIconNew panelName
        False -> Gio.themedIconNewWithDefaultFallbacks name
      View.ImgSGIcon <$> Gio.toIcon themed
    forCustomTheme themePath = do
      theme <- liftIO $ customIconTheme themePath
      pixbuf <- MaybeT $ UI.iconThemeLoadIcon theme panelName (iconSizePx size) loadFlags
      pure (View.ImgSPixbuf pixbuf)
    directPath path = do
      fileIcon <- Gio.fileNewForPath path >>= Gio.fileIconNew
      View.ImgSGIcon <$> Gio.toIcon fileIcon

imgInfoSet :: UI.IconSize -> HS.ImageInfo -> MaybeT IO View.ImageSet
imgInfoSet size imgs = do
  (width, height, colors) : _ <- pure $ sortOn (\(_, height, _) -> abs (height - iconSizePx size)) imgs
  guard $ BS.length colors == fromIntegral (width * height * nSample)
  -- MAYBE convert to RGBA?
  bytes <- Glib.bytesNew (Just colors)
  pixbuf <- Gdk.pixbufNewFromBytes bytes Gdk.ColorspaceRgb True 8 width height (width * nSample)
  pure $ View.ImgSPixbuf pixbuf
  where
    nSample = 4

iconSizePx :: UI.IconSize -> Int32
iconSizePx = \case
  UI.IconSizeMenu -> 16
  UI.IconSizeSmallToolbar -> 16
  UI.IconSizeLargeToolbar -> 24
  UI.IconSizeButton -> 16
  UI.IconSizeDnd -> 24
  UI.IconSizeDialog -> 32
  _ -> 8