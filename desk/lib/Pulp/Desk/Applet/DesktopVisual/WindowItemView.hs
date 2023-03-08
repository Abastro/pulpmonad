{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pulp.Desk.Applet.DesktopVisual.WindowItemView (
  View (..),
  getPriority,
  setPriority,
  setGIcon,
  setRawIcons,
  setActivate,
  setTitle,
  setStates,
  clickSource,
) where

import Data.GI.Base.Attributes qualified as GI
import Data.GI.Base.BasicTypes qualified as GI
import Data.GI.Base.GObject qualified as GI
import Data.GI.Base.GParamSpec qualified as GI
import Data.GI.Base.Overloading qualified as GI
import Data.Text qualified as T
import GHC.OverloadedLabels
import GHC.Records
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gtk.Objects.Button qualified as Gtk
import GI.Gtk.Objects.Image qualified as Gtk
import GI.Gtk.Structs.WidgetClass qualified as Gtk
import Pulp.Desk.PulpPath
import Pulp.Desk.Reactive.Entry
import Pulp.Desk.System.X11.WMStatus qualified as X11
import Pulp.Desk.UI.Commons qualified as Gtk
import Pulp.Desk.UI.Pixbufs qualified as Gtk
import Pulp.Desk.UI.Reactive qualified as Gtk
import Pulp.Desk.UI.Styles qualified as Gtk

-- MAYBE Use image internal to the button
-- Essentially, a button with priority. Select in CSS by "button.windowitem".
newtype View = AsView (GI.ManagedPtr View)

instance GI.TypedObject View where
  glibType :: IO GI.GType
  glibType = GI.registerGType AsView
instance GI.GObject View

type instance GI.ParentTypes View = Gtk.Button ': GI.ParentTypes Gtk.Button
instance GI.HasParentTypes View

type instance GI.AttributeList View = GI.AttributeList Gtk.Widget
instance GI.HasAttributeList View

instance
  ( info ~ Gtk.ResolveWidgetMethod t View
  , GI.OverloadedMethod info View p
  ) =>
  IsLabel t (View -> p)
  where
  fromLabel = GI.overloadedMethod @info
instance
  ( info ~ Gtk.ResolveWidgetMethod t View
  , GI.OverloadedMethod info View p
  , HasField t View p
  ) =>
  HasField t View p
  where
  getField = GI.overloadedMethod @info

data Private = MkPrivate
  { priority :: !Int
  , windowIcon :: !Gtk.Image
  }

instance GI.DerivedGObject View where
  type GObjectParentType View = Gtk.Button
  type GObjectPrivateData View = Private

  objectTypeName :: T.Text
  objectTypeName = T.pack "WindowItemView"

  objectClassInit :: GI.GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "desktop-visualizer" </> "window-item.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile

    widgetClass.bindTemplateChildFull (T.pack "window-icon") True 0

    GI.gobjectInstallCIntProperty @View
      gClass
      GI.CIntPropertyInfo
        { name = T.pack "priority"
        , nick = T.pack "Priority"
        , blurb = T.pack "Priority of the window item"
        , defaultValue = 0
        , setter = \widget v -> GI.gobjectModifyPrivateData widget $
            \dat -> dat{priority = fromIntegral v}
        , getter = \widget -> do
            (\priv -> fromIntegral priv.priority) <$> GI.gobjectGetPrivateData widget
        , flags = Nothing
        , minValue = Just 0
        , maxValue = Nothing
        }

    -- Handling signal is sadly not supported for now.

    widgetClass.setCssName (T.pack "button")

  objectInstanceInit :: GI.GObjectClass -> View -> IO Private
  objectInstanceInit _gClass inst = do
    inst.initTemplate
    windowIcon <- Gtk.templateChild inst (T.pack "window-icon") Gtk.Image

    inst.getStyleContext >>= flip #addClass (T.pack "windowitem")

    pure MkPrivate{priority = 0, windowIcon}

-- Too lazy to add overloaded labels for property

-- | Gets priority, should be called from UI thread.
getPriority :: View -> IO Int
getPriority view = (\priv -> priv.priority) <$> GI.gobjectGetPrivateData view

-- | Sets priority, should be called from UI thread.
setPriority :: View -> Int -> IO ()
setPriority view priority = GI.gobjectModifyPrivateData view $ \dat -> dat{priority}

setGIcon :: View -> Sink Gio.Icon
setGIcon view gic = Gtk.uiSingleRun $ do
  MkPrivate{windowIcon} <- GI.gobjectGetPrivateData view
  GI.set windowIcon [#gicon GI.:= gic]

setRawIcons :: View -> Sink [Gtk.RawIcon]
setRawIcons view icons = Gtk.uiSingleRun $ do
  MkPrivate{windowIcon} <- GI.gobjectGetPrivateData view
  iconSize <- toEnum . fromIntegral <$> GI.get windowIcon #iconSize
  Gtk.iconsChoosePixbuf (Gtk.iconSizePx iconSize) Gtk.argbTorgba icons >>= \case
    Just scaled -> GI.set windowIcon [#pixbuf GI.:= scaled]
    Nothing -> GI.set windowIcon [#iconName GI.:= T.pack "image-missing"]

setActivate :: View -> Sink Bool
setActivate view flag = Gtk.uiSingleRun $ do
  ctxt <- view.getStyleContext
  (if flag then ctxt.addClass else ctxt.removeClass) (T.pack "active")

setTitle :: View -> Sink T.Text
setTitle view title = Gtk.uiSingleRun $ do
  GI.set view [#tooltipText GI.:= title]

setStates :: View -> Sink [X11.WMStateEx]
setStates view states = Gtk.uiSingleRun $ do
  view.getStyleContext >>= Gtk.updateCssClass asClass states
  where
    asClass = \case
      X11.WinHidden -> T.pack "hidden"
      X11.WinDemandAttention -> T.pack "demanding"

clickSource :: View -> Source ()
clickSource view =
  Gtk.onSource (view `GI.asA` Gtk.Button) #clicked $ \handler -> do
    handler ()
