{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Applet.SysTray.TrayItemView (
  TrayItemView (..),
  MouseButton (..),
  TrayItemInput (..),
  TrayItemIcon (..),
  itemSetInputHandler,
  itemSetIcon,
  itemSetOverlay,
  itemShowPopup,
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString qualified as BS
import Data.Foldable
import Data.GI.Base.Attributes
import Data.GI.Base.BasicTypes
import Data.GI.Base.GObject
import Data.GI.Base.Overloading
import Data.GI.Base.Signals
import Data.Int
import Data.Text qualified as T
import GHC.OverloadedLabels
import GI.DbusmenuGtk3.Objects.Menu qualified as DBus
import GI.Gdk.Objects.Screen qualified as Gdk
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gio.Objects.FileIcon qualified as Gio
import GI.Gtk.Objects.EventBox qualified as Gtk
import GI.Gtk.Objects.IconTheme qualified as Gtk
import GI.Gtk.Objects.Image qualified as Gtk
import GI.Gtk.Objects.Menu qualified as Gtk
import GI.Gtk.Structs.WidgetClass qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Pixbufs qualified as Gtk
import System.Pulp.PulpPath

newtype TrayItemView = TrayItemView (ManagedPtr TrayItemView)

instance TypedObject TrayItemView where
  glibType :: IO GType
  glibType = registerGType TrayItemView
instance GObject TrayItemView

type instance ParentTypes TrayItemView = Gtk.EventBox ': ParentTypes Gtk.EventBox
instance HasParentTypes TrayItemView

-- Disallow box/container methods
instance
  ( info ~ Gtk.ResolveWidgetMethod t TrayItemView
  , OverloadedMethod info TrayItemView p
  ) =>
  IsLabel t (TrayItemView -> p)
  where
  fromLabel = overloadedMethod @info

data TrayItemPrivate = TrayItemPrivate
  { trayIcon :: !Gtk.Image
  , trayOverlay :: !Gtk.Image
  }

instance DerivedGObject TrayItemView where
  -- MAYBE Inherit button and its styling
  type GObjectParentType TrayItemView = Gtk.EventBox
  type GObjectPrivateData TrayItemView = TrayItemPrivate

  objectTypeName :: T.Text
  objectTypeName = T.pack "TrayItemView"

  objectClassInit :: GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "system-tray" </> "tray-item.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile

    #bindTemplateChildFull widgetClass (T.pack "tray-icon") True 0
    #bindTemplateChildFull widgetClass (T.pack "tray-overlay") True 0

    #setCssName widgetClass (T.pack "TrayItemView")

  objectInstanceInit :: GObjectClass -> TrayItemView -> IO TrayItemPrivate
  objectInstanceInit _gClass inst = do
    #initTemplate inst
    trayIcon <- Gtk.templateChild inst (T.pack "tray-icon") Gtk.Image
    trayOverlay <- Gtk.templateChild inst (T.pack "tray-overlay") Gtk.Image

    -- Make scrollable, eventbox is not scrollable by default
    #addEvents inst [Gtk.EventMaskScrollMask]

    pure TrayItemPrivate{..}

data MouseButton = MouseLeft | MouseMiddle | MouseRight
data TrayItemInput
  = TrayItemScroll !Gtk.ScrollDirection
  | TrayItemClick !MouseButton !Int32 !Int32

-- | Icon information from the tray item.
data TrayItemIcon = TrayItemIcon
  { itemThemePath :: Maybe String
  , itemIconName :: Maybe T.Text
  , itemIconInfo :: [(Int32, Int32, BS.ByteString)]
  }

itemSetInputHandler :: TrayItemView -> (TrayItemInput -> IO ()) -> IO ()
itemSetInputHandler item handler = do
  on (item `asA` Gtk.Widget) #buttonPressEvent $ \event -> do
    xRoot <- round <$> get event #xRoot
    yRoot <- round <$> get event #yRoot
    get event #button >>= \case
      1 -> True <$ handler (TrayItemClick MouseLeft xRoot yRoot)
      2 -> True <$ handler (TrayItemClick MouseMiddle xRoot yRoot)
      3 -> True <$ handler (TrayItemClick MouseRight xRoot yRoot)
      _ -> pure False
  on (item `asA` Gtk.Widget) #scrollEvent $ \event -> do
    direction <- get event #direction
    True <$ handler (TrayItemScroll direction)
  pure ()

itemSetIcon :: TrayItemView -> TrayItemIcon -> IO ()
itemSetIcon item icon = do
  TrayItemPrivate{trayIcon} <- gobjectGetPrivateData item
  setTrayIcon trayIcon icon

itemSetOverlay :: TrayItemView -> TrayItemIcon -> IO ()
itemSetOverlay item icon = do
  TrayItemPrivate{trayOverlay} <- gobjectGetPrivateData item
  setTrayIcon trayOverlay icon

itemShowPopup :: TrayItemView -> DBus.Menu -> IO ()
itemShowPopup item menu = do
  Gtk.menuPopupAtWidget menu item Gtk.GravitySouthWest Gtk.GravityNorthWest Nothing

data IconData = ByGIcon Gio.Icon | ByPixbuf Gtk.Pixbuf

setTrayIcon :: Gtk.Image -> TrayItemIcon -> IO ()
setTrayIcon image icon = do
  pixelSize <- get image #pixelSize
  itemIconAsSet pixelSize icon >>= \case
    Just (ByGIcon gic) -> set image [#gicon := gic]
    Just (ByPixbuf pbuf) -> set image [#pixbuf := pbuf]
    Nothing -> set image [#iconName := T.pack "image-missing"]

itemIconAsSet :: Int32 -> TrayItemIcon -> IO (Maybe IconData)
itemIconAsSet pixelSize TrayItemIcon{..} =
  runMaybeT $
    maybe empty (imgNameSet pixelSize itemThemePath) itemIconName
      <|> imgInfoSet pixelSize itemIconInfo

-- MaybeT due to caller
customIconTheme :: String -> MaybeT IO Gtk.IconTheme
customIconTheme themePath = do
  custom <- Gtk.iconThemeNew
  Gdk.screenGetDefault >>= traverse_ (#setScreen custom)
  #appendSearchPath custom themePath

  defTheme <- Gtk.iconThemeGetDefault
  #getSearchPath defTheme >>= traverse_ (#appendSearchPath custom)
  pure custom

imgNameSet :: Int32 -> Maybe String -> T.Text -> MaybeT IO IconData
imgNameSet pixelSize mayTheme name = do
  guard $ not (T.null name)
  -- Icon should be freedesktop-compliant icon name.
  case mayTheme of
    Just themePath | themePath /= "" -> forCustomTheme themePath <|> directPath themePath name
    _ -> withDefTheme
  where
    panelName = name <> T.pack "-panel" -- Looks up first with "-panel" suffix
    loadFlags = [Gtk.IconLookupFlagsUseBuiltin, Gtk.IconLookupFlagsGenericFallback]
    withDefTheme = Gtk.iconThemeGetDefault >>= setPixbuf
    forCustomTheme themePath = customIconTheme themePath >>= setPixbuf
    directPath themePath name = do
      fpath <- Gio.fileNewForPath themePath >>= flip Gio.fileGetChild (T.unpack name)
      fileIcon <- Gio.fileIconNew fpath
      ByGIcon <$> Gio.toIcon fileIcon
    setPixbuf theme = do
      fmap ByPixbuf . MaybeT $ Gtk.themeLoadIcon theme panelName pixelSize loadFlags

-- NB: Why does `id` work? SNI is ARGB, GTK is RGBA.
imgInfoSet :: Int32 -> [(Int32, Int32, BS.ByteString)] -> MaybeT IO IconData
imgInfoSet pixelSize imgs = do
  fmap ByPixbuf . MaybeT $ Gtk.iconsChoosePixbuf pixelSize id icons
  where
    icons = asRawIcon <$> imgs
    asRawIcon (iconWidth, iconHeight, iconColors) = Gtk.RawIcon{..}
