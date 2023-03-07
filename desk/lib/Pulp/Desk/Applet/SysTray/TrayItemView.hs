{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pulp.Desk.Applet.SysTray.TrayItemView (
  View (..),
  MouseButton (..),
  MouseClick (..),
  ScrollDir (..),
  TrayItemIcon (..),
  clickSource,
  scrollSource,
  setIcon,
  setOverlay,
  setTooltip,
  setMenu,
  showPopup,
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString qualified as BS
import Data.Foldable
import Data.GI.Base.Attributes qualified as GI
import Data.GI.Base.BasicTypes qualified as GI
import Data.GI.Base.Constructible qualified as GI
import Data.GI.Base.GObject qualified as GI
import Data.GI.Base.Overloading qualified as GI
import Data.Int
import Data.Text qualified as T
import GHC.OverloadedLabels
import GHC.Records
import GI.DbusmenuGtk3.Objects.Menu qualified as DMenu
import GI.Gdk.Objects.Screen qualified as Gdk
import GI.Gdk.Unions.Event qualified as Gdk
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gio.Objects.FileIcon qualified as Gio
import GI.Gtk.Objects.EventBox qualified as Gtk
import GI.Gtk.Objects.IconTheme qualified as Gtk
import GI.Gtk.Objects.Image qualified as Gtk
import GI.Gtk.Objects.Menu qualified as Gtk
import GI.Gtk.Structs.WidgetClass qualified as Gtk
import Pulp.Desk.PulpPath
import Pulp.Desk.Reactive.Entry
import Pulp.Desk.UI.Commons qualified as Gtk
import Pulp.Desk.UI.Pixbufs qualified as Gtk
import Pulp.Desk.UI.Reactive qualified as Gtk

newtype View = AsView (GI.ManagedPtr View)

instance GI.TypedObject View where
  glibType :: IO GI.GType
  glibType = GI.registerGType AsView
instance GI.GObject View

type instance GI.ParentTypes View = Gtk.EventBox ': GI.ParentTypes Gtk.EventBox
instance GI.HasParentTypes View

-- Disallow box/container methods
instance
  ( info ~ Gtk.ResolveWidgetMethod t View
  , GI.OverloadedMethod info View p
  ) =>
  IsLabel t (View -> p)
  where
  fromLabel = GI.overloadedMethod @info
instance
  ( info ~ Gtk.ResolveWidgetMethod t View
  , GI.OverloadedMethod info View p
  , HasField t View p
  ) =>
  HasField t View p
  where
  getField = GI.overloadedMethod @info

data TrayItemPrivate = TrayItemPrivate
  { trayIcon :: !Gtk.Image
  , trayOverlay :: !Gtk.Image
  , trayMenu :: Maybe Gtk.Menu
  }

instance GI.DerivedGObject View where
  -- MAYBE Inherit button and its styling
  type GObjectParentType View = Gtk.EventBox
  type GObjectPrivateData View = TrayItemPrivate

  objectTypeName :: T.Text
  objectTypeName = T.pack "TrayItemView"

  objectClassInit :: GI.GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "system-tray" </> "tray-item.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile

    widgetClass.bindTemplateChildFull (T.pack "tray-icon") True 0
    widgetClass.bindTemplateChildFull (T.pack "tray-overlay") True 0

    widgetClass.setCssName (T.pack "TrayItemView")

  objectInstanceInit :: GI.GObjectClass -> View -> IO TrayItemPrivate
  objectInstanceInit _gClass inst = do
    inst.initTemplate
    trayIcon <- Gtk.templateChild inst (T.pack "tray-icon") Gtk.Image
    trayOverlay <- Gtk.templateChild inst (T.pack "tray-overlay") Gtk.Image

    -- Make scrollable, eventbox is not scrollable by default
    inst.addEvents [Gtk.EventMaskScrollMask]

    pure TrayItemPrivate{trayMenu = Nothing, ..}

data MouseButton = MouseLeft | MouseMiddle | MouseRight
data MouseClick = MouseClickOf (Maybe Gdk.Event) !Int32 !Int32 !MouseButton
data ScrollDir = ScrollUp | ScrollDown | ScrollLeft | ScrollRight

-- | Icon information from the tray item.
data TrayItemIcon = TrayItemIcon
  { itemThemePath :: Maybe String
  , itemIconName :: Maybe T.Text
  , itemIconInfo :: [(Int32, Int32, BS.ByteString)]
  }

-- | This one comes with Gdk Event for implementation reasons.
clickSource :: View -> Source MouseClick
clickSource view =
  Gtk.onSource (view `GI.asA` Gtk.Widget) #buttonPressEvent $ \handler event -> do
    evUnion <- Gtk.getCurrentEvent
    xRoot <- round <$> GI.get event #xRoot
    yRoot <- round <$> GI.get event #yRoot
    let click = MouseClickOf evUnion xRoot yRoot
    GI.get event #button >>= \case
      1 -> True <$ handler (click MouseLeft)
      2 -> True <$ handler (click MouseMiddle)
      3 -> True <$ handler (click MouseRight)
      _ -> pure False

scrollSource :: View -> Source ScrollDir
scrollSource view =
  Gtk.onSource (view `GI.asA` Gtk.Widget) #scrollEvent $ \handler event -> do
    GI.get event #direction >>= \case
      Gtk.ScrollDirectionUp -> True <$ handler ScrollUp
      Gtk.ScrollDirectionDown -> True <$ handler ScrollDown
      Gtk.ScrollDirectionLeft -> True <$ handler ScrollLeft
      Gtk.ScrollDirectionRight -> True <$ handler ScrollRight
      _ -> pure False

setIcon :: View -> Sink TrayItemIcon
setIcon item icon = Gtk.uiSingleRun $ do
  TrayItemPrivate{trayIcon} <- GI.gobjectGetPrivateData item
  setTrayIcon True trayIcon icon

setOverlay :: View -> Sink TrayItemIcon
setOverlay item icon = Gtk.uiSingleRun $ do
  TrayItemPrivate{trayOverlay} <- GI.gobjectGetPrivateData item
  setTrayIcon False trayOverlay icon

setTooltip :: View -> Sink (Maybe T.Text)
setTooltip view tooltip = Gtk.uiSingleRun $ do
  view.setTooltipText tooltip

setMenu :: View -> Sink (T.Text, T.Text)
setMenu view (bus, path) = Gtk.uiSingleRun $ do
  menu <- DMenu.menuNew bus path
  GI.gobjectModifyPrivateData view $ \priv -> priv{trayMenu = Just $ menu `GI.asA` Gtk.Menu}

showPopup :: View -> Sink (Maybe Gdk.Event)
showPopup view event = Gtk.uiSingleRun $ do
  TrayItemPrivate{trayMenu} <- GI.gobjectGetPrivateData view
  for_ trayMenu $ \menu -> do
    menu.popupAtWidget view Gtk.GravitySouthWest Gtk.GravityNorthWest event

{- Internal procedures here -}

data IconData = ByGIcon Gio.Icon | ByPixbuf Gtk.Pixbuf

setTrayIcon :: Bool -> Gtk.Image -> TrayItemIcon -> IO ()
setTrayIcon showMissing image icon = do
  pixelSize <- GI.get image #pixelSize
  itemIconAsSet pixelSize icon >>= \case
    Just (ByGIcon gic) -> GI.set image [#gicon GI.:= gic]
    Just (ByPixbuf pbuf) -> GI.set image [#pixbuf GI.:= pbuf]
    Nothing -> when showMissing $ GI.set image [#iconName GI.:= T.pack "image-missing"]

itemIconAsSet :: Int32 -> TrayItemIcon -> IO (Maybe IconData)
itemIconAsSet pixelSize TrayItemIcon{..} =
  runMaybeT $
    maybe empty (imgNameSet pixelSize itemThemePath) itemIconName
      <|> imgInfoSet pixelSize itemIconInfo

-- MaybeT due to caller
customIconTheme :: String -> MaybeT IO Gtk.IconTheme
customIconTheme themePath = do
  custom <- GI.new Gtk.IconTheme []
  Gdk.screenGetDefault >>= traverse_ custom.setScreen
  custom.appendSearchPath themePath

  defTheme <- Gtk.iconThemeGetDefault
  defTheme.getSearchPath >>= traverse_ custom.appendSearchPath
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
    loadFlags = [Gtk.IconLookupFlagsUseBuiltin, Gtk.IconLookupFlagsGenericFallback, Gtk.IconLookupFlagsForceSize]
    withDefTheme = Gtk.iconThemeGetDefault >>= setPixbuf
    forCustomTheme themePath = customIconTheme themePath >>= setPixbuf
    directPath themePath name =
      ByGIcon <$> do
        themeDir <- Gio.fileNewForPath themePath
        iconPath <- themeDir.getChild (T.unpack name)
        fileIcon <- GI.new Gio.FileIcon [#file GI.:= iconPath]
        pure (fileIcon `GI.asA` Gio.Icon)
    setPixbuf theme =
      ByPixbuf <$> do
        MaybeT $ Gtk.themeLoadIcon theme panelName pixelSize loadFlags

-- NB: Why does `id` work? SNI is ARGB, GTK is RGBA.
imgInfoSet :: Int32 -> [(Int32, Int32, BS.ByteString)] -> MaybeT IO IconData
imgInfoSet pixelSize imgs = do
  fmap ByPixbuf . MaybeT $ Gtk.iconsChoosePixbuf pixelSize id icons
  where
    icons = asRawIcon <$> imgs
    asRawIcon (iconWidth, iconHeight, iconColors) = Gtk.RawIcon{..}
