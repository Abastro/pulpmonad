{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Applet.DesktopVisual.WindowItemView (
  WindowItemView (..),
  getPriority,
  setPriority,
  windowSetIcon,
  windowClickAct,
) where

import Data.GI.Base.Attributes
import Data.GI.Base.BasicTypes
import Data.GI.Base.GObject
import Data.GI.Base.GParamSpec
import Data.GI.Base.Overloading
import Data.GI.Base.Signals
import Data.Text qualified as T
import GHC.OverloadedLabels
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gtk.Objects.Button qualified as Gtk
import GI.Gtk.Objects.Image qualified as Gtk
import GI.Gtk.Structs.WidgetClass qualified as Gtk
import Gtk.Commons qualified as Gtk
import System.Pulp.PulpPath
import View.Imagery

-- MAYBE Use image internal to the button
-- Essentially, a button with priority. Select in CSS by "button.windowitem".
newtype WindowItemView = WindowItemView (ManagedPtr WindowItemView)

instance TypedObject WindowItemView where
  glibType :: IO GType
  glibType = registerGType WindowItemView
instance GObject WindowItemView

type instance ParentTypes WindowItemView = Gtk.Button ': ParentTypes Gtk.Button
instance HasParentTypes WindowItemView

instance
  ( info ~ Gtk.ResolveWidgetMethod t WindowItemView
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
    Gtk.setTemplateFromGFile widgetClass uiFile

    #bindTemplateChildFull widgetClass (T.pack "window-icon") True 0

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

    -- Handling signal is sadly not supported for now.
    #setCssName widgetClass (T.pack "button")

  objectInstanceInit :: GObjectClass -> WindowItemView -> IO WindowItemPrivate
  objectInstanceInit _gClass inst = do
    #initTemplate inst
    windowIcon <- Gtk.templateChild inst (T.pack "window-icon") Gtk.Image

    #getStyleContext inst >>= flip #addClass (T.pack "windowitem")

    pure WindowItemPrivate{priority = 0, windowIcon}

-- Too lazy to add labels for property
getPriority :: WindowItemView -> IO Int
getPriority window = priority <$> gobjectGetPrivateData window

setPriority :: WindowItemView -> Int -> IO ()
setPriority window priority = gobjectModifyPrivateData window $ \dat -> dat{priority}

windowSetIcon :: WindowItemView -> ImageSet -> IO ()
windowSetIcon window iconSet = do
  WindowItemPrivate{windowIcon} <- gobjectGetPrivateData window
  case iconSet of
    ImgSName name -> set windowIcon [#iconName := name]
    ImgSGIcon gic -> set windowIcon [#gicon := gic]
    ImgSPixbuf pbuf -> set windowIcon [#pixbuf := pbuf]

windowClickAct :: WindowItemView -> IO () -> IO ()
windowClickAct window act = do
  on (window `asA` Gtk.Button) #clicked act
  pure ()
