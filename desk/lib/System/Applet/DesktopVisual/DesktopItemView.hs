{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}

module System.Applet.DesktopVisual.DesktopItemView () where

import Data.GI.Base.BasicTypes
import Data.GI.Base.GObject
import Data.GI.Base.Overloading
import Data.GI.Gtk qualified as Gtk
import Data.Text qualified as T
import GI.Gtk.Objects.Box qualified as Gtk
import GI.Gtk.Objects.FlowBox qualified as Gtk
import GI.Gtk.Objects.Label qualified as Gtk
import Gtk.Commons qualified as Gtk
import GHC.OverloadedLabels
import qualified GI.Gio.Interfaces.File as Gio
import System.Pulp.PulpPath
import qualified GI.Gio as Gio

-- Thin wrapper around the template for desktop item
newtype DesktopItemView = DesktopItemView (ManagedPtr DesktopItemView)

instance TypedObject DesktopItemView where
  glibType :: IO GType
  glibType = registerGType DesktopItemView
instance GObject DesktopItemView

type instance ParentTypes DesktopItemView = Gtk.Box ': ParentTypes Gtk.Box
instance HasParentTypes DesktopItemView

-- Disallow box methods
instance
  ( info ~ Gtk.ResolveContainerMethod t DesktopItemView
  , OverloadedMethod info DesktopItemView p
  ) =>
  IsLabel t (DesktopItemView -> p)
  where
  fromLabel = overloadedMethod @info

data DesktopItemPrivate = DesktopItemPrivate
  { desktopLabel :: !Gtk.Label
  , desktopContainer :: !Gtk.FlowBox
  }

instance DerivedGObject DesktopItemView where
  type GObjectParentType DesktopItemView = Gtk.Box
  type GObjectPrivateData DesktopItemView = DesktopItemPrivate

  objectTypeName :: T.Text
  objectTypeName = T.pack "DesktopItemView"

  objectClassInit :: GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "desktop-visualizer" </> "desktop-item.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile
    undefined

  objectInstanceInit :: GObjectClass -> DesktopItemView -> IO DesktopItemPrivate
  objectInstanceInit _gClass inst = do
    #initTemplate inst
    desktopLabel <- Gtk.templateChild inst (T.pack "desktop-label") Gtk.Label
    desktopContainer <- Gtk.templateChild inst (T.pack "desktop-container") Gtk.FlowBox

    pure DesktopItemPrivate{..}
