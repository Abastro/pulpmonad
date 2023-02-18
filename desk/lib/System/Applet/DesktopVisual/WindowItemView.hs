{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Applet.DesktopVisual.WindowItemView (
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

import Control.Event.Entry
import Data.GI.Base.Attributes
import Data.GI.Base.BasicTypes
import Data.GI.Base.GObject
import Data.GI.Base.GParamSpec
import Data.GI.Base.Overloading
import Data.Text qualified as T
import GHC.OverloadedLabels
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gtk.Objects.Button qualified as Gtk
import GI.Gtk.Objects.Image qualified as Gtk
import GI.Gtk.Structs.WidgetClass qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Pixbufs qualified as Gtk
import Gtk.Reactive qualified as Gtk
import Gtk.Styles qualified as Gtk
import Gtk.Task qualified as Gtk
import Status.X11.WMStatus
import System.Pulp.PulpPath

-- MAYBE Use image internal to the button
-- Essentially, a button with priority. Select in CSS by "button.windowitem".
newtype View = AsView (ManagedPtr View)

instance TypedObject View where
  glibType :: IO GType
  glibType = registerGType AsView
instance GObject View

type instance ParentTypes View = Gtk.Button ': ParentTypes Gtk.Button
instance HasParentTypes View

instance
  ( info ~ Gtk.ResolveWidgetMethod t View
  , OverloadedMethod info View p
  ) =>
  IsLabel t (View -> p)
  where
  fromLabel = overloadedMethod @info

data Private = MkPrivate
  { priority :: !Int
  , windowIcon :: !Gtk.Image
  }

instance DerivedGObject View where
  type GObjectParentType View = Gtk.Button
  type GObjectPrivateData View = Private

  objectTypeName :: T.Text
  objectTypeName = T.pack "WindowItemView"

  objectClassInit :: GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "desktop-visualizer" </> "window-item.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile

    #bindTemplateChildFull widgetClass (T.pack "window-icon") True 0

    gobjectInstallCIntProperty @View
      gClass
      CIntPropertyInfo
        { name = T.pack "priority"
        , nick = T.pack "Priority"
        , blurb = T.pack "Priority of the window item"
        , defaultValue = 0
        , setter = \widget v -> gobjectModifyPrivateData widget $
            \dat -> dat{priority = fromIntegral v}
        , getter = \widget -> do
            (\priv -> fromIntegral priv.priority) <$> gobjectGetPrivateData widget
        , flags = Nothing
        , minValue = Just 0
        , maxValue = Nothing
        }

    -- Handling signal is sadly not supported for now.

    #setCssName widgetClass (T.pack "button")

  objectInstanceInit :: GObjectClass -> View -> IO Private
  objectInstanceInit _gClass inst = do
    #initTemplate inst
    windowIcon <- Gtk.templateChild inst (T.pack "window-icon") Gtk.Image

    #getStyleContext inst >>= flip #addClass (T.pack "windowitem")

    pure MkPrivate{priority = 0, windowIcon}

-- Too lazy to add overloaded labels for property

-- | Gets priority, should be called from UI thread.
getPriority :: View -> IO Int
getPriority view = (\priv -> priv.priority) <$> gobjectGetPrivateData view

-- | Sets priority, should be called from UI thread.
setPriority :: View -> Int -> IO ()
setPriority view priority = gobjectModifyPrivateData view $ \dat -> dat{priority}

setGIcon :: View -> Sink Gio.Icon
setGIcon view gic = Gtk.uiSingleRun $ do
  MkPrivate{windowIcon} <- gobjectGetPrivateData view
  set windowIcon [#gicon := gic]

setRawIcons :: View -> Sink [Gtk.RawIcon]
setRawIcons view icons = Gtk.uiSingleRun $ do
  MkPrivate{windowIcon} <- gobjectGetPrivateData view
  iconSize <- toEnum . fromIntegral <$> get windowIcon #iconSize
  Gtk.iconsChoosePixbuf (Gtk.iconSizePx iconSize) Gtk.argbTorgba icons >>= \case
    Just scaled -> set windowIcon [#pixbuf := scaled]
    Nothing -> set windowIcon [#iconName := T.pack "image-missing"]

setActivate :: View -> Sink Bool
setActivate view flag = Gtk.uiSingleRun $ do
  ctxt <- #getStyleContext view
  (if flag then #addClass else #removeClass) ctxt (T.pack "active")

setTitle :: View -> Sink T.Text
setTitle view title = Gtk.uiSingleRun $ do
  set (view `asA` Gtk.Widget) [#tooltipText := title]

setStates :: View -> Sink [WMStateEx]
setStates view states = Gtk.uiSingleRun $ do
  #getStyleContext view >>= Gtk.updateCssClass asClass states
  where
    asClass = \case
      WinHidden -> T.pack "hidden"
      WinDemandAttention -> T.pack "demanding"

clickSource :: View -> Source ()
clickSource view =
  Gtk.onSource (view `asA` Gtk.Button) #clicked $ \handler -> do
    handler ()
