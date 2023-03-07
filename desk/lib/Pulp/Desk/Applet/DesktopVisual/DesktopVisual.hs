{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pulp.Desk.Applet.DesktopVisual.DesktopVisual (
  View (..),
  insertDesktop,
  removeDesktop,
) where

import Data.GI.Base.BasicTypes qualified as GI
import Data.GI.Base.GObject qualified as GI
import Data.GI.Base.Overloading qualified as GI
import Data.Text qualified as T
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gtk.Objects.Box qualified as Gtk
import GI.Gtk.Structs.WidgetClass qualified as Gtk
import Pulp.Desk.Applet.DesktopVisual.DesktopItemView qualified as DeskView
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

data Private = MkPrivate

instance GI.DerivedGObject View where
  type GObjectParentType View = Gtk.Box
  type GObjectPrivateData View = Private

  objectTypeName :: T.Text
  objectTypeName = T.pack "DesktopVisualizer"

  objectClassInit :: GI.GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "desktop-visualizer" </> "desktop-visualizer.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile

    widgetClass.setCssName (T.pack "desktopvisualizer")

  objectInstanceInit :: GI.GObjectClass -> View -> IO Private
  objectInstanceInit _gClass inst = do
    (inst `GI.asA` Gtk.Widget).initTemplate
    pure MkPrivate

insertDesktop :: View -> DeskView.View -> IO ()
insertDesktop view desktop = Gtk.uiSingleRun $ do
  (view `GI.asA` Gtk.Box).add desktop

removeDesktop :: View -> DeskView.View -> IO ()
removeDesktop view desktop = Gtk.uiSingleRun $ do
  (view `GI.asA` Gtk.Box).remove desktop
