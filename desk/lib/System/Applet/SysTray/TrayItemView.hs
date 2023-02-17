{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Applet.SysTray.TrayItemView (
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
) where

import Control.Applicative
import Control.Event.Entry
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString qualified as BS
import Data.Foldable
import Data.GI.Base.Attributes
import Data.GI.Base.BasicTypes
import Data.GI.Base.GObject
import Data.GI.Base.Overloading
import Data.Int
import Data.Text qualified as T
import GHC.OverloadedLabels
import GI.Gdk.Objects.Screen qualified as Gdk
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gio.Objects.FileIcon qualified as Gio
import GI.Gtk.Objects.EventBox qualified as Gtk
import GI.Gtk.Objects.IconTheme qualified as Gtk
import GI.Gtk.Objects.Image qualified as Gtk
import GI.Gtk.Structs.WidgetClass qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Pixbufs qualified as Gtk
import Gtk.Reactive qualified as Gtk
import Gtk.Task qualified as Gtk
import System.Pulp.PulpPath
import Data.GI.Base.Constructible
newtype View = AsView (ManagedPtr View)

instance TypedObject View where
  glibType :: IO GType
  glibType = registerGType AsView
instance GObject View

type instance ParentTypes View = Gtk.EventBox ': ParentTypes Gtk.EventBox
instance HasParentTypes View

-- Disallow box/container methods
instance
  ( info ~ Gtk.ResolveWidgetMethod t View
  , OverloadedMethod info View p
  ) =>
  IsLabel t (View -> p)
  where
  fromLabel = overloadedMethod @info

data TrayItemPrivate = TrayItemPrivate
  { trayIcon :: !Gtk.Image
  , trayOverlay :: !Gtk.Image
  }

instance DerivedGObject View where
  -- MAYBE Inherit button and its styling
  type GObjectParentType View = Gtk.EventBox
  type GObjectPrivateData View = TrayItemPrivate

  objectTypeName :: T.Text
  objectTypeName = T.pack "TrayItemView"

  objectClassInit :: GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "system-tray" </> "tray-item.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile

    #bindTemplateChildFull widgetClass (T.pack "tray-icon") True 0
    #bindTemplateChildFull widgetClass (T.pack "tray-overlay") True 0

    #setCssName widgetClass (T.pack "TrayItemView")

  objectInstanceInit :: GObjectClass -> View -> IO TrayItemPrivate
  objectInstanceInit _gClass inst = do
    #initTemplate inst
    trayIcon <- Gtk.templateChild inst (T.pack "tray-icon") Gtk.Image
    trayOverlay <- Gtk.templateChild inst (T.pack "tray-overlay") Gtk.Image

    -- Make scrollable, eventbox is not scrollable by default
    #addEvents inst [Gtk.EventMaskScrollMask]

    pure TrayItemPrivate{..}

data MouseButton = MouseLeft | MouseMiddle | MouseRight
data MouseClick = MouseClickOf !Int32 !Int32 !MouseButton
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
  Gtk.onSource (view `asA` Gtk.Widget) #buttonPressEvent $ \handler event -> do
    xRoot <- round <$> get event #xRoot
    yRoot <- round <$> get event #yRoot
    let mouseClick = MouseClickOf xRoot yRoot
    get event #button >>= \case
      1 -> True <$ handler (mouseClick MouseLeft)
      2 -> True <$ handler (mouseClick MouseMiddle)
      3 -> True <$ handler (mouseClick MouseRight)
      _ -> pure False

scrollSource :: View -> Source ScrollDir
scrollSource view =
  Gtk.onSource (view `asA` Gtk.Widget) #scrollEvent $ \handler event -> do
    get event #direction >>= \case
      Gtk.ScrollDirectionUp -> True <$ handler ScrollUp
      Gtk.ScrollDirectionDown -> True <$ handler ScrollDown
      Gtk.ScrollDirectionLeft -> True <$ handler ScrollLeft
      Gtk.ScrollDirectionRight -> True <$ handler ScrollRight
      _ -> pure False

setIcon :: View -> Sink TrayItemIcon
setIcon item icon = Gtk.uiSingleRun $ do
  TrayItemPrivate{trayIcon} <- gobjectGetPrivateData item
  setTrayIcon True trayIcon icon

setOverlay :: View -> Sink TrayItemIcon
setOverlay item icon = Gtk.uiSingleRun $ do
  TrayItemPrivate{trayOverlay} <- gobjectGetPrivateData item
  setTrayIcon False trayOverlay icon

setTooltip :: View -> Sink (Maybe T.Text)
setTooltip view tooltip = Gtk.uiSingleRun $ do
  #setTooltipText (view `asA` Gtk.Widget) tooltip

{- Internal procedures here -}

data IconData = ByGIcon Gio.Icon | ByPixbuf Gtk.Pixbuf

setTrayIcon :: Bool -> Gtk.Image -> TrayItemIcon -> IO ()
setTrayIcon showMissing image icon = do
  pixelSize <- get image #pixelSize
  itemIconAsSet pixelSize icon >>= \case
    Just (ByGIcon gic) -> set image [#gicon := gic]
    Just (ByPixbuf pbuf) -> set image [#pixbuf := pbuf]
    Nothing -> when showMissing $ set image [#iconName := T.pack "image-missing"]

itemIconAsSet :: Int32 -> TrayItemIcon -> IO (Maybe IconData)
itemIconAsSet pixelSize TrayItemIcon{..} =
  runMaybeT $
    maybe empty (imgNameSet pixelSize itemThemePath) itemIconName
      <|> imgInfoSet pixelSize itemIconInfo

-- MaybeT due to caller
customIconTheme :: String -> MaybeT IO Gtk.IconTheme
customIconTheme themePath = do
  custom <- new Gtk.IconTheme []
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
    -- Uses pixbuf, because somehow lookup directory is different.
    withDefTheme = Gtk.iconThemeGetDefault >>= setPixbuf
    forCustomTheme themePath = customIconTheme themePath >>= setPixbuf
    directPath themePath name = ByGIcon <$> do
      themeDir <- Gio.fileNewForPath themePath
      iconPath <- #getChild themeDir (T.unpack name)
      fileIcon <- new Gio.FileIcon [#file := iconPath]
      pure (fileIcon `asA` Gio.Icon)
    setPixbuf theme = ByPixbuf <$> do
      MaybeT $ Gtk.themeLoadIcon theme panelName pixelSize loadFlags

-- NB: Why does `id` work? SNI is ARGB, GTK is RGBA.
imgInfoSet :: Int32 -> [(Int32, Int32, BS.ByteString)] -> MaybeT IO IconData
imgInfoSet pixelSize imgs = do
  fmap ByPixbuf . MaybeT $ Gtk.iconsChoosePixbuf pixelSize id icons
  where
    icons = asRawIcon <$> imgs
    asRawIcon (iconWidth, iconHeight, iconColors) = Gtk.RawIcon{..}
