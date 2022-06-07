module System.Pulp.Applet.SysTray.View (
  SysTray,
  SysTrayOp (..),
  sysTrayWidget,
  sysTrayNew,
  sysTrayCtrl,
  TrayItem,
  TrayItemOp (..),
  TrayItemInput (..),
  MouseButton (..),
  trayItemWidget,
  trayItemNew,
  trayItemCtrl,
  TrayItemIcon (..),
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Int
import Data.Maybe
import Data.Text qualified as T
import GI.DbusmenuGtk3.Objects.Menu qualified as DBus
import GI.Gdk.Flags qualified as Gdk
import GI.Gdk.Objects.Screen qualified as Gdk
import GI.Gdk.Structs.EventButton qualified as Gdk
import GI.Gdk.Structs.EventScroll qualified as Gdk
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gio.Objects.FileIcon qualified as Gio
import GI.Gtk.Objects.Box qualified as Gtk
import GI.Gtk.Objects.EventBox qualified as Gtk
import GI.Gtk.Objects.IconTheme qualified as Gtk
import GI.Gtk.Objects.Menu qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.Pixbufs qualified as Gtk
import Gtk.Styles qualified as Gtk
import View.Imagery qualified as View

data SysTray = SysTray
  { sysTrayWid :: !Gtk.Widget
  , sysTrayBox :: !Gtk.Box
  , sysTrayAlignBegin :: !Bool
  }

data SysTrayOp = TrayAddItem !TrayItem | TrayRemoveItem !TrayItem

sysTrayWidget :: SysTray -> Gtk.Widget
sysTrayWidget SysTray{sysTrayWid} = sysTrayWid

sysTrayNew :: MonadIO m => Gtk.Orientation -> Bool -> m SysTray
sysTrayNew orientation sysTrayAlignBegin = do
  sysTrayBox <- Gtk.boxNew orientation 0
  sysTrayWid <- Gtk.toWidget sysTrayBox
  Gtk.widgetGetStyleContext sysTrayWid >>= flip Gtk.styleContextAddClass (T.pack "system-tray-area")
  pure SysTray{..}

sysTrayCtrl :: MonadIO m => SysTray -> SysTrayOp -> m ()
sysTrayCtrl SysTray{..} = \case
  TrayAddItem TrayItem{trayItemWid} -> do
    boxPack sysTrayBox trayItemWid False False 0
    Gtk.widgetShowAll trayItemWid
  TrayRemoveItem TrayItem{trayItemWid} -> do
    Gtk.widgetHide trayItemWid
    Gtk.widgetDestroy trayItemWid
  where
    boxPack = if sysTrayAlignBegin then Gtk.boxPackStart else Gtk.boxPackEnd

data TrayItem = TrayItem
  { trayItemWid :: !Gtk.Widget
  , trayItemSize :: !Gtk.IconSize
  , trayItemIcon :: !View.ImageDyn
  , trayItemOverlay :: !View.ImageDyn
  }

data TrayItemOp
  = ItemSetInputHandler !(TrayItemInput -> IO ())
  | ItemSetIcon !TrayItemIcon
  | ItemSetOverlay !TrayItemIcon
  | ItemSetTooltip !(Maybe T.Text)
  | ItemShowPopup !DBus.Menu

data MouseButton = MouseLeft | MouseMiddle | MouseRight
data TrayItemInput
  = TrayItemScroll !Gtk.ScrollDirection
  | TrayItemClick !MouseButton !Int32 !Int32

trayItemWidget :: TrayItem -> Gtk.Widget
trayItemWidget TrayItem{trayItemWid} = trayItemWid

trayItemNew :: MonadIO m => Gtk.IconSize -> m TrayItem
trayItemNew trayItemSize = do
  trayItemIcon <- View.imageDynNew trayItemSize
  trayItemOverlay <- View.imageDynNew trayItemSize
  overlay <- Gtk.overlayed (View.imageDynWidget trayItemIcon) [View.imageDynWidget trayItemOverlay]

  interactive <- Gtk.eventBoxNew
  Gtk.widgetAddEvents interactive [Gdk.EventMaskScrollMask]
  Gtk.containerAdd interactive overlay
  trayItemWid <- Gtk.toWidget interactive
  Gtk.widgetGetStyleContext trayItemWid >>= flip Gtk.styleContextAddClass (T.pack "system-tray-item")

  pure TrayItem{..}

trayItemCtrl :: MonadIO m => TrayItem -> TrayItemOp -> m ()
trayItemCtrl TrayItem{..} = \case
  ItemSetTooltip tooltip -> Gtk.widgetSetTooltipText trayItemWid tooltip
  ItemSetIcon icon -> itemIconAsSet trayItemSize icon >>= View.imageDynSetImg trayItemIcon
  ItemSetOverlay icon -> itemIconAsSet trayItemSize icon >>= View.imageDynSetImg trayItemOverlay
  ItemSetInputHandler handler -> do
    Gtk.onWidgetButtonPressEvent trayItemWid $ \event -> do
      xRoot <- round <$> Gdk.getEventButtonXRoot event
      yRoot <- round <$> Gdk.getEventButtonYRoot event
      Gdk.getEventButtonButton event >>= \case
        1 -> True <$ handler (TrayItemClick MouseLeft xRoot yRoot)
        2 -> True <$ handler (TrayItemClick MouseMiddle xRoot yRoot)
        3 -> True <$ handler (TrayItemClick MouseRight xRoot yRoot)
        _ -> pure False
    Gtk.onWidgetScrollEvent trayItemWid $ \event -> do
      direction <- Gdk.getEventScrollDirection event
      True <$ handler (TrayItemScroll direction)
    pure ()
  ItemShowPopup menu -> do
    Gtk.menuPopupAtWidget menu trayItemWid Gtk.GravitySouthWest Gtk.GravityNorthWest Nothing

-- | Icon information from the tray item.
data TrayItemIcon = TrayItemIcon
  { itemThemePath :: Maybe String
  , itemIconName :: Maybe T.Text
  , itemIconInfo :: [(Int32, Int32, BS.ByteString)]
  }

itemIconAsSet :: MonadIO m => Gtk.IconSize -> TrayItemIcon -> m View.ImageSet
itemIconAsSet iconSize TrayItemIcon{..} = do
  iconSet <-
    liftIO . runMaybeT $
      maybe empty (imgNameSet iconSize itemThemePath) itemIconName
        <|> imgInfoSet iconSize itemIconInfo
  pure $ fromMaybe (View.ImgSName $ T.pack "missing") iconSet

customIconTheme :: MonadIO m => String -> m Gtk.IconTheme
customIconTheme themePath = do
  custom <- Gtk.iconThemeNew
  Gdk.screenGetDefault >>= traverse_ (Gtk.iconThemeSetScreen custom)
  Gtk.iconThemeAppendSearchPath custom themePath

  defTheme <- Gtk.iconThemeGetDefault
  Gtk.iconThemeGetSearchPath defTheme >>= traverse_ (Gtk.iconThemeAppendSearchPath custom)
  pure custom

imgNameSet :: Gtk.IconSize -> Maybe String -> T.Text -> MaybeT IO View.ImageSet
imgNameSet size mayTheme name = do
  guard $ not (T.null name)
  -- Icon should be freedesktop-compliant icon name.
  let nonEmp p = p <$ guard (not $ null p)
  case mayTheme >>= nonEmp of
    Nothing -> withDefTheme
    Just themePath -> forCustomTheme themePath <|> directPath themePath name
  where
    panelName = name <> T.pack "-panel" -- Looks up first with "-panel" suffix
    loadFlags = [Gtk.IconLookupFlagsUseBuiltin, Gtk.IconLookupFlagsGenericFallback]
    withDefTheme = Gtk.iconThemeGetDefault >>= setPixbuf
    forCustomTheme themePath = customIconTheme themePath >>= setPixbuf
    directPath themePath name = do
      fpath <- Gio.fileNewForPath themePath >>= flip Gio.fileGetChild (T.unpack name)
      fileIcon <- Gio.fileIconNew fpath
      View.ImgSGIcon <$> Gio.toIcon fileIcon

    setPixbuf theme = do
      pixbuf <- MaybeT $ Gtk.iconThemeLoadIcon theme panelName (Gtk.iconSizePx size) loadFlags
      pure (View.ImgSPixbuf pixbuf)

-- NB: Why does `id` work? SNI is ARGB, GTK is RGBA.
imgInfoSet :: Gtk.IconSize -> [(Int32, Int32, BS.ByteString)] -> MaybeT IO View.ImageSet
imgInfoSet size imgs = do
  pixbuf <- MaybeT $ Gtk.iconsChoosePixbuf (Gtk.iconSizePx size) id icons
  pure $ View.ImgSPixbuf pixbuf
  where
    icons = asRawIcon <$> imgs
    asRawIcon (iconWidth, iconHeight, iconColors) = Gtk.RawIcon{..}
