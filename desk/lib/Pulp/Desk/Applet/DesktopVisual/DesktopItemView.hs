{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pulp.Desk.Applet.DesktopVisual.DesktopItemView (
  View (..),
  insertWindow,
  removeWindow,
  reflectPriority,
  setLabel,
  setVisible,
  DesktopState (..),
  setState,
  clickSource,
) where

import Data.GI.Base.Attributes qualified as GI
import Data.GI.Base.BasicTypes qualified as GI
import Data.GI.Base.Constructible qualified as GI
import Data.GI.Base.GObject qualified as GI
import Data.GI.Base.ManagedPtr qualified as GI
import Data.GI.Base.Overloading qualified as GI
import Data.Int
import Data.Text qualified as T
import GHC.OverloadedLabels
import GHC.Records
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gtk.Objects.EventBox qualified as Gtk
import GI.Gtk.Objects.FlowBox qualified as Gtk
import GI.Gtk.Objects.FlowBoxChild qualified as Gtk
import GI.Gtk.Objects.Label qualified as Gtk
import GI.Gtk.Structs.WidgetClass qualified as Gtk
import Pulp.Desk.Applet.DesktopVisual.WindowItemView qualified as WinView
import Pulp.Desk.PulpPath
import Pulp.Desk.Reactive.Entry
import Pulp.Desk.System.X11.WMStatus
import Pulp.Desk.UI.Commons qualified as Gtk
import Pulp.Desk.UI.Reactive qualified as Gtk
import Pulp.Desk.UI.Styles qualified as Gtk

-- Desktop item which has windows sorted via priority
newtype View = AsView (GI.ManagedPtr View)

instance GI.TypedObject View where
  glibType :: IO GI.GType
  glibType = GI.registerGType AsView
instance GI.GObject View

type instance GI.ParentTypes View = Gtk.EventBox ': GI.ParentTypes Gtk.EventBox
instance GI.HasParentTypes View
type instance GI.SignalList View = GI.SignalList Gtk.Widget

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
  { desktopLabel :: !Gtk.Label
  , desktopContainer :: !Gtk.FlowBox
  }

instance GI.DerivedGObject View where
  type GObjectParentType View = Gtk.EventBox
  type GObjectPrivateData View = Private

  objectTypeName :: T.Text
  objectTypeName = T.pack "DesktopItemView"

  objectClassInit :: GI.GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "desktop-visualizer" </> "desktop-item.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile

    widgetClass.bindTemplateChildFull (T.pack "desktop-label") True 0
    widgetClass.bindTemplateChildFull (T.pack "desktop-container") True 0

    widgetClass.setCssName (T.pack "desktopitem")

  objectInstanceInit :: GI.GObjectClass -> View -> IO Private
  objectInstanceInit _gClass inst = do
    inst.initTemplate
    desktopLabel <- Gtk.templateChild inst (T.pack "desktop-label") Gtk.Label
    desktopContainer <- Gtk.templateChild inst (T.pack "desktop-container") Gtk.FlowBox

    -- Sort function is pre-set
    desktopContainer.setSortFunc (Just sortFunc)

    pure MkPrivate{..}

sortFunc :: Gtk.FlowBoxChild -> Gtk.FlowBoxChild -> IO Int32
sortFunc childA childB = do
  priorA <- priorityOf childA
  priorB <- priorityOf childB
  pure $ case priorA `compare` priorB of
    LT -> -1
    EQ -> 0
    GT -> 1
  where
    -- This means we need to refer to WindowItemView anyway..
    priorityOf :: Gtk.FlowBoxChild -> IO Int
    priorityOf child = do
      Just wid <- child.getChild
      win <- GI.unsafeCastTo WinView.AsView wid
      WinView.getPriority win

insertWindow :: View -> WinView.View -> IO ()
insertWindow desktop window = Gtk.uiSingleRun $ do
  MkPrivate{desktopContainer} <- GI.gobjectGetPrivateData desktop
  -- FlowBox requires FlowBoxChild
  winChild <- GI.new Gtk.FlowBoxChild [#child GI.:= window]
  desktopContainer.add winChild

removeWindow :: View -> WinView.View -> IO ()
removeWindow desktop window = Gtk.uiSingleRun $ do
  MkPrivate{desktopContainer} <- GI.gobjectGetPrivateData desktop
  -- Gets the parent, FlowBoxChild
  Just winChild <- traverse (GI.unsafeCastTo Gtk.FlowBoxChild) =<< #getParent window
  -- And remove both parent-child deps
  winChild.remove window
  desktopContainer.remove winChild

-- | Reflect the priorities to the sort as priorities change.
--
-- This is here since in the end, priorities are usually updated in batch.
-- (Also, notifying FlowBoxChild can introduce coupling)
reflectPriority :: View -> IO ()
reflectPriority desktop = Gtk.uiSingleRun $ do
  MkPrivate{desktopContainer} <- GI.gobjectGetPrivateData desktop
  desktopContainer.invalidateSort

setLabel :: View -> Sink T.Text
setLabel desktop txt = Gtk.uiSingleRun $ do
  MkPrivate{desktopLabel} <- GI.gobjectGetPrivateData desktop
  GI.set desktopLabel [#label GI.:= txt]

setVisible :: View -> Sink Bool
setVisible desktop =
  Gtk.uiSingleRun . \case
    True -> desktop.showAll
    False -> desktop.hide

setState :: View -> Sink DesktopState
setState desktop state = Gtk.uiSingleRun $ do
  desktop.getStyleContext >>= Gtk.updateCssClass asClass [state]
  where
    asClass = \case
      DeskActive -> T.pack "active"
      DeskVisible -> T.pack "visible"
      DeskHidden -> T.pack "hidden"

clickSource :: View -> Source ()
clickSource desktop =
  Gtk.onSource desktop #buttonReleaseEvent $ \handler btn -> do
    GI.get btn #button >>= \case
      1 -> True <$ handler ()
      _ -> pure False
