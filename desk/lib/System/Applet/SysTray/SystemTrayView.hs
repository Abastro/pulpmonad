{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Applet.SysTray.SystemTrayView (
  SystemTrayView (..),
  PackAt (..),
  traySetOrientation,
  traySetPackAt,
  trayAddItem,
  trayRemoveItem,
) where

import Data.GI.Base.Attributes
import Data.GI.Base.BasicTypes
import Data.GI.Base.GObject
import Data.GI.Base.Overloading
import Data.Text qualified as T
import GHC.OverloadedLabels
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gtk.Objects.Box qualified as Gtk
import GI.Gtk.Structs.WidgetClass qualified as Gtk
import Gtk.Commons qualified as Gtk
import System.Applet.SysTray.TrayItemView
import System.Pulp.PulpPath

newtype SystemTrayView = SystemTrayView (ManagedPtr SystemTrayView)

instance TypedObject SystemTrayView where
  glibType :: IO GType
  glibType = registerGType SystemTrayView
instance GObject SystemTrayView

type instance ParentTypes SystemTrayView = Gtk.Box ': ParentTypes Gtk.Box
instance HasParentTypes SystemTrayView

-- Disallow box/container methods
instance
  ( info ~ Gtk.ResolveWidgetMethod t SystemTrayView
  , OverloadedMethod info SystemTrayView p
  ) =>
  IsLabel t (SystemTrayView -> p)
  where
  fromLabel = overloadedMethod @info

data PackAt = PackStart | PackEnd
newtype TrayPrivate = TrayPrivate PackAt

instance DerivedGObject SystemTrayView where
  type GObjectParentType SystemTrayView = Gtk.Box
  type GObjectPrivateData SystemTrayView = TrayPrivate

  objectTypeName :: T.Text
  objectTypeName = T.pack "SystemTrayView"

  objectClassInit :: GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "system-tray" </> "system-tray.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile

    #setCssName widgetClass (T.pack "SystemTrayView")

  objectInstanceInit :: GObjectClass -> SystemTrayView -> IO TrayPrivate
  objectInstanceInit _gClass inst = do
    #initTemplate inst
    pure (TrayPrivate PackStart)

traySetOrientation :: SystemTrayView -> Gtk.Orientation -> IO ()
traySetOrientation tray orient = do
  set (tray `asA` Gtk.Box) [#orientation := orient]

traySetPackAt :: SystemTrayView -> PackAt -> IO ()
traySetPackAt tray at = do
  gobjectSetPrivateData tray (TrayPrivate at)

trayAddItem :: SystemTrayView -> TrayItemView -> IO ()
trayAddItem tray item = do
  TrayPrivate at <- gobjectGetPrivateData tray
  case at of
    PackStart -> #packStart (tray `asA` Gtk.Box) item False False 0
    PackEnd -> #packEnd (tray `asA` Gtk.Box) item False False 0
  #showAll item

trayRemoveItem :: SystemTrayView -> TrayItemView -> IO ()
trayRemoveItem tray item = do
  #hide item
  #remove (tray `asA` Gtk.Box) item
