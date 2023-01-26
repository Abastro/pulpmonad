{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Applet.DesktopVisual.DesktopVisual (
  DesktopVisual (..),
  addDesktops,
  cutDesktopToCount,
) where

import Data.Foldable
import Data.GI.Base.BasicTypes
import Data.GI.Base.GObject
import Data.GI.Base.Overloading
import Data.Text qualified as T
import Data.Vector qualified as V
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

addDesktops :: DesktopVisual -> V.Vector DesktopItemView -> IO ()
addDesktops view newDesks = for_ newDesks $ \desktop -> do
  #add (view `asA` Gtk.Box) desktop
  #showAll desktop

cutDesktopToCount :: DesktopVisual -> Int -> IO ()
cutDesktopToCount view cnt = do
  let box = view `asA` Gtk.Box
  desktops <- #getChildren box
  -- Removes except for first cnt desktops
  for_ (drop cnt desktops) $ \desktop -> do
    #hide desktop
    #remove box desktop
