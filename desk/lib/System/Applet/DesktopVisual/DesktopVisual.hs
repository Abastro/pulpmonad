{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Applet.DesktopVisual.DesktopVisual (
  DesktopVisual (..),
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
import Gtk.Commons qualified as Gtk
import System.Applet.DesktopVisual.DesktopItemView
import System.Pulp.PulpPath

newtype DesktopVisual = DesktopVisual (ManagedPtr DesktopVisual)

instance TypedObject DesktopVisual where
  glibType :: IO GType
  glibType = registerGType DesktopVisual
instance GObject DesktopVisual

type instance ParentTypes DesktopVisual = Gtk.Box ': ParentTypes Gtk.Box
instance HasParentTypes DesktopVisual

-- Disallow box/container methods
instance
  ( info ~ Gtk.ResolveWidgetMethod t DesktopVisual
  , OverloadedMethod info DesktopVisual p
  ) =>
  IsLabel t (DesktopVisual -> p)
  where
  fromLabel = overloadedMethod @info

data VisualPrivate = VisualPrivate

instance DerivedGObject DesktopVisual where
  type GObjectParentType DesktopVisual = Gtk.Box
  type GObjectPrivateData DesktopVisual = VisualPrivate

  objectTypeName :: T.Text
  objectTypeName = T.pack "DesktopVisualizer"

  objectClassInit :: GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "desktop-visualizer" </> "desktop-visualizer.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile

    #setCssName widgetClass (T.pack "desktopvisualizer")

  objectInstanceInit :: GObjectClass -> DesktopVisual -> IO VisualPrivate
  objectInstanceInit _gClass inst = do
    #initTemplate inst
    pure VisualPrivate

insertDesktop :: DesktopVisual -> DesktopItemView -> IO ()
insertDesktop view desktop = do
  #add (view `asA` Gtk.Box) desktop
  #showAll desktop

removeDesktop :: DesktopVisual -> DesktopItemView -> IO ()
removeDesktop view desktop = do
  #hide desktop
  #remove (view `asA` Gtk.Box) desktop
