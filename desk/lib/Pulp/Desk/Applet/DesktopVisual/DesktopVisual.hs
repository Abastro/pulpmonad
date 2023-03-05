{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pulp.Desk.Applet.DesktopVisual.DesktopVisual (
  View (..),
  insertDesktop,
  removeDesktop,
) where

import Data.GI.Base.BasicTypes
import Data.GI.Base.GObject
import Data.GI.Base.Overloading
import Data.Text qualified as T
import GHC.OverloadedLabels
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gtk.Objects.Box qualified as Gtk
import GI.Gtk.Structs.WidgetClass qualified as Gtk
import Pulp.Desk.Applet.DesktopVisual.DesktopItemView qualified as DeskView
import Pulp.Desk.PulpPath
import Pulp.Desk.UI.Commons qualified as Gtk
import Pulp.Desk.UI.Task qualified as Gtk

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

data Private = MkPrivate

instance DerivedGObject View where
  type GObjectParentType View = Gtk.Box
  type GObjectPrivateData View = Private

  objectTypeName :: T.Text
  objectTypeName = T.pack "DesktopVisualizer"

  objectClassInit :: GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "desktop-visualizer" </> "desktop-visualizer.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile

    #setCssName widgetClass (T.pack "desktopvisualizer")

  objectInstanceInit :: GObjectClass -> View -> IO Private
  objectInstanceInit _gClass inst = do
    #initTemplate inst
    pure MkPrivate

insertDesktop :: View -> DeskView.View -> IO ()
insertDesktop view desktop = Gtk.uiSingleRun $ do
  #add (view `asA` Gtk.Box) desktop

removeDesktop :: View -> DeskView.View -> IO ()
removeDesktop view desktop = Gtk.uiSingleRun $ do
  #remove (view `asA` Gtk.Box) desktop
