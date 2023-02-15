{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Applet.SysTray.SystemTrayView (
  View (..),
  PackAt (..),
  setOrientation,
  setPackAt,
  addItem,
  removeItem,
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
import System.Applet.SysTray.TrayItemView qualified as ItemView
import System.Pulp.PulpPath
import qualified Gtk.Task as Gtk

newtype View = AsView (ManagedPtr View)

instance TypedObject View where
  glibType :: IO GType
  glibType = registerGType AsView
instance GObject View

type instance ParentTypes View = Gtk.Box ': ParentTypes Gtk.Box
instance HasParentTypes View

-- Disallow box/container methods
instance
  ( info ~ Gtk.ResolveWidgetMethod t View
  , OverloadedMethod info View p
  ) =>
  IsLabel t (View -> p)
  where
  fromLabel = overloadedMethod @info

data PackAt = PackStart | PackEnd
newtype TrayPrivate = TrayPrivate PackAt

instance DerivedGObject View where
  type GObjectParentType View = Gtk.Box
  type GObjectPrivateData View = TrayPrivate

  objectTypeName :: T.Text
  objectTypeName = T.pack "SystemTrayView"

  objectClassInit :: GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "system-tray" </> "system-tray.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile

    #setCssName widgetClass (T.pack "SystemTrayView")

  objectInstanceInit :: GObjectClass -> View -> IO TrayPrivate
  objectInstanceInit _gClass inst = do
    #initTemplate inst
    pure (TrayPrivate PackStart)

setOrientation :: View -> Gtk.Orientation -> IO ()
setOrientation tray orient = Gtk.uiSingleRun $ do
  set (tray `asA` Gtk.Box) [#orientation := orient]

setPackAt :: View -> PackAt -> IO ()
setPackAt tray at = Gtk.uiSingleRun $ do
  gobjectSetPrivateData tray (TrayPrivate at)

addItem :: View -> ItemView.View -> IO ()
addItem tray item = Gtk.uiSingleRun $ do
  TrayPrivate at <- gobjectGetPrivateData tray
  case at of
    PackStart -> #packStart (tray `asA` Gtk.Box) item False False 0
    PackEnd -> #packEnd (tray `asA` Gtk.Box) item False False 0
  #showAll item

removeItem :: View -> ItemView.View -> IO ()
removeItem tray item = Gtk.uiSingleRun $ do
  #hide item
  #remove (tray `asA` Gtk.Box) item
