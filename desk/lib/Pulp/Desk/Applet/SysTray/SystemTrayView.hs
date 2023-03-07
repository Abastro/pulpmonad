{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pulp.Desk.Applet.SysTray.SystemTrayView (
  View (..),
  PackAt (..),
  setOrientation,
  setPackAt,
  addItem,
  removeItem,
) where

import Data.GI.Base.Attributes qualified as GI
import Data.GI.Base.BasicTypes qualified as GI
import Data.GI.Base.GObject qualified as GI
import Data.GI.Base.Overloading qualified as GI
import Data.Text qualified as T
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gtk.Objects.Box qualified as Gtk
import GI.Gtk.Structs.WidgetClass qualified as Gtk
import Pulp.Desk.Applet.SysTray.TrayItemView qualified as ItemView
import Pulp.Desk.PulpPath
import Pulp.Desk.UI.Commons qualified as Gtk
import Pulp.Desk.UI.Reactive qualified as Gtk

newtype View = AsView (GI.ManagedPtr View)

instance GI.TypedObject View where
  glibType :: IO GI.GType
  glibType = GI.registerGType AsView
instance GI.GObject View

type instance GI.ParentTypes View = Gtk.Box ': GI.ParentTypes Gtk.Box
instance GI.HasParentTypes View

data PackAt = PackStart | PackEnd
newtype TrayPrivate = TrayPrivate PackAt

instance GI.DerivedGObject View where
  type GObjectParentType View = Gtk.Box
  type GObjectPrivateData View = TrayPrivate

  objectTypeName :: T.Text
  objectTypeName = T.pack "SystemTrayView"

  objectClassInit :: GI.GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "system-tray" </> "system-tray.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile

    widgetClass.setCssName (T.pack "SystemTrayView")

  objectInstanceInit :: GI.GObjectClass -> View -> IO TrayPrivate
  objectInstanceInit _gClass inst = do
    (inst `GI.asA` Gtk.Widget).initTemplate
    pure (TrayPrivate PackStart)

setOrientation :: View -> Gtk.Orientation -> IO ()
setOrientation tray orient = Gtk.uiSingleRun $ do
  GI.set (tray `GI.asA` Gtk.Box) [#orientation GI.:= orient]

setPackAt :: View -> PackAt -> IO ()
setPackAt tray at = Gtk.uiSingleRun $ do
  GI.gobjectSetPrivateData tray (TrayPrivate at)

addItem :: View -> ItemView.View -> IO ()
addItem tray item = Gtk.uiSingleRun $ do
  TrayPrivate at <- GI.gobjectGetPrivateData tray
  let trayBox = tray `GI.asA` Gtk.Box
  case at of
    PackStart -> trayBox.packStart item False False 0
    PackEnd -> trayBox.packEnd item False False 0
  item.showAll

removeItem :: View -> ItemView.View -> IO ()
removeItem tray item = Gtk.uiSingleRun $ do
  item.hide
  (tray `GI.asA` Gtk.Box).remove item
