{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Applet.DesktopVisual.WindowItemView (
  WindowItemView (..),
  winItemGetPriority,
  winItemSetPriority,
  winItemSetIcon,
) where

import Data.GI.Base.Attributes
import Data.GI.Base.BasicTypes
import Data.GI.Base.GObject
import Data.GI.Base.GParamSpec
import Data.GI.Base.Overloading
import Data.Text qualified as T
import GHC.OverloadedLabels
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gio.Objects.Cancellable qualified as Gio
import GI.Gtk.Objects.Button qualified as Gtk
import GI.Gtk.Objects.Image qualified as Gtk
import GI.Gtk.Structs.WidgetClass qualified as Gtk
import Gtk.Commons qualified as Gtk
import System.Pulp.PulpPath

newtype WindowItemView = WindowItemView (ManagedPtr WindowItemView)

instance TypedObject WindowItemView where
  glibType :: IO GType
  glibType = registerGType WindowItemView
instance GObject WindowItemView

type instance ParentTypes WindowItemView = Gtk.Button ': ParentTypes Gtk.Button
instance HasParentTypes WindowItemView

instance
  ( info ~ Gtk.ResolveButtonMethod t WindowItemView
  , OverloadedMethod info WindowItemView p
  ) =>
  IsLabel t (WindowItemView -> p)
  where
  fromLabel = overloadedMethod @info

data WindowItemPrivate = WindowItemPrivate
  { priority :: !Int
  , windowIcon :: !Gtk.Image
  }

instance DerivedGObject WindowItemView where
  type GObjectParentType WindowItemView = Gtk.Button
  type GObjectPrivateData WindowItemView = WindowItemPrivate

  objectTypeName :: T.Text
  objectTypeName = T.pack "WindowItemView"

  objectClassInit :: GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "desktop-visualizer" </> "window-item.ui")
    (bytes, _) <- #loadBytes uiFile (Nothing @Gio.Cancellable)
    #setTemplate widgetClass bytes

    -- Icon as an internal child
    #bindTemplateChildFull widgetClass (T.pack "window-item-icon") True 0

    gobjectInstallCIntProperty @WindowItemView
      gClass
      CIntPropertyInfo
        { name = T.pack "priority"
        , nick = T.pack "Priority"
        , blurb = T.pack "Priority of the window item"
        , defaultValue = 0
        , setter = \widget v -> gobjectModifyPrivateData widget $
            \dat -> dat{priority = fromIntegral v}
        , getter = \widget -> do
            fromIntegral . priority <$> gobjectGetPrivateData widget
        , flags = Nothing
        , minValue = Just 0
        , maxValue = Nothing
        }

    -- Currently, this is just a button with a property

    -- Handling signal is sadly not supported for now.
    #setCssName widgetClass (T.pack "windowitem")

  objectInstanceInit :: GObjectClass -> WindowItemView -> IO WindowItemPrivate
  objectInstanceInit _gClass inst = do
    #initTemplate inst
    windowIcon <- Gtk.templateChild inst (T.pack "window-icon") Gtk.Image

    pure WindowItemPrivate{priority = 0, windowIcon}

-- Too lazy to add labels for property
winItemGetPriority :: WindowItemView -> IO Int
winItemGetPriority widget = priority <$> gobjectGetPrivateData widget

winItemSetPriority :: WindowItemView -> Int -> IO ()
winItemSetPriority widget priority = gobjectModifyPrivateData widget $ \dat -> dat{priority}

winItemSetIcon :: WindowItemView -> T.Text -> IO ()
winItemSetIcon widget iconName = do
  image <- windowIcon <$> gobjectGetPrivateData widget
  set image [#iconName := iconName]
