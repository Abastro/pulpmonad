{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Applet.DesktopVisual.WindowItemView (
  WindowItemView (..),
) where

import Data.GI.Base.BasicTypes
import Data.GI.Base.GObject
import Data.GI.Base.Overloading
import Data.Text qualified as T
import GI.Gtk.Objects.Bin qualified as Gtk
import GI.Gtk.Objects.Image qualified as Gtk
import GI.Gtk.Structs.WidgetClass qualified as Gtk
import Gtk.Commons qualified as Gtk

newtype WindowItemView = WindowItemView (ManagedPtr WindowItemView)

instance TypedObject WindowItemView where
  glibType :: IO GType
  glibType = registerGType WindowItemView
instance GObject WindowItemView

type instance ParentTypes WindowItemView = Gtk.Bin ': ParentTypes Gtk.Bin
instance HasParentTypes WindowItemView

data WindowItemPrivate = WindowItemPrivate
  { priority :: !Int
  , icon :: !Gtk.Image
  }

instance DerivedGObject WindowItemView where
  type GObjectParentType WindowItemView = Gtk.Bin
  type GObjectPrivateData WindowItemView = WindowItemPrivate

  objectTypeName :: T.Text
  objectTypeName = T.pack "WindowItemView"

  objectClassInit :: GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    #setTemplate widgetClass undefined -- TODO
    -- Icon as an internal child
    #bindTemplateChildFull widgetClass (T.pack "window-item-icon") True 0
    -- TODO Handle signals
    #setCssName widgetClass (T.pack "windowitem")

  objectInstanceInit :: GObjectClass -> WindowItemView -> IO WindowItemPrivate
  objectInstanceInit _gClass inst = do
    widget <- Gtk.toWidget inst
    #initTemplate widget
    -- TODO Commons machinary for this
    imageType <- glibType @Gtk.Image
    #getTemplateChild widget imageType (T.pack "window-item-icon")

    pure WindowItemPrivate{priority = 0, icon = undefined}
