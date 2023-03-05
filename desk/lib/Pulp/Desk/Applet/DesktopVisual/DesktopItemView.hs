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

import Data.GI.Base.Attributes
import Data.GI.Base.BasicTypes
import Data.GI.Base.Constructible
import Data.GI.Base.GObject
import Data.GI.Base.ManagedPtr
import Data.GI.Base.Overloading
import Data.Int
import Data.Text qualified as T
import GHC.OverloadedLabels
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
newtype View = AsView (ManagedPtr View)

instance TypedObject View where
  glibType :: IO GType
  glibType = registerGType AsView
instance GObject View

type instance ParentTypes View = Gtk.EventBox ': ParentTypes Gtk.EventBox
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
  { desktopLabel :: !Gtk.Label
  , desktopContainer :: !Gtk.FlowBox
  }

instance DerivedGObject View where
  type GObjectParentType View = Gtk.EventBox
  type GObjectPrivateData View = Private

  objectTypeName :: T.Text
  objectTypeName = T.pack "DesktopItemView"

  objectClassInit :: GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "desktop-visualizer" </> "desktop-item.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile

    #bindTemplateChildFull widgetClass (T.pack "desktop-label") True 0
    #bindTemplateChildFull widgetClass (T.pack "desktop-container") True 0

    #setCssName widgetClass (T.pack "desktopitem")

  objectInstanceInit :: GObjectClass -> View -> IO Private
  objectInstanceInit _gClass inst = do
    #initTemplate inst
    desktopLabel <- Gtk.templateChild inst (T.pack "desktop-label") Gtk.Label
    desktopContainer <- Gtk.templateChild inst (T.pack "desktop-container") Gtk.FlowBox

    -- Sort function is pre-set
    #setSortFunc desktopContainer (Just sortFunc)

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
      Just wid <- #getChild child
      win <- unsafeCastTo WinView.AsView wid
      WinView.getPriority win

insertWindow :: View -> WinView.View -> IO ()
insertWindow desktop window = Gtk.uiSingleRun $ do
  MkPrivate{desktopContainer} <- gobjectGetPrivateData desktop
  -- FlowBox requires FlowBoxChild
  winChild <- new Gtk.FlowBoxChild [#child := window]
  #add desktopContainer winChild

removeWindow :: View -> WinView.View -> IO ()
removeWindow desktop window = Gtk.uiSingleRun $ do
  MkPrivate{desktopContainer} <- gobjectGetPrivateData desktop
  -- Gets the parent, FlowBoxChild
  Just winChild <- traverse (unsafeCastTo Gtk.FlowBoxChild) =<< #getParent window
  -- And remove both parent-child deps
  #remove winChild window
  #remove desktopContainer winChild

-- | Reflect the priorities to the sort as priorities change.
--
-- This is here since in the end, priorities are usually updated in batch.
-- (Also, notifying FlowBoxChild can introduce coupling)
reflectPriority :: View -> IO ()
reflectPriority desktop = Gtk.uiSingleRun $ do
  MkPrivate{desktopContainer} <- gobjectGetPrivateData desktop
  #invalidateSort desktopContainer

setLabel :: View -> Sink T.Text
setLabel desktop txt = Gtk.uiSingleRun $ do
  MkPrivate{desktopLabel} <- gobjectGetPrivateData desktop
  set desktopLabel [#label := txt]

setVisible :: View -> Sink Bool
setVisible desktop =
  Gtk.uiSingleRun . \case
    True -> #showAll desktop
    False -> #hide desktop

setState :: View -> Sink DesktopState
setState desktop state = Gtk.uiSingleRun $ do
  #getStyleContext desktop >>= Gtk.updateCssClass asClass [state]
  where
    asClass = \case
      DeskActive -> T.pack "active"
      DeskVisible -> T.pack "visible"
      DeskHidden -> T.pack "hidden"

clickSource :: View -> Source ()
clickSource desktop =
  Gtk.onSource (desktop `asA` Gtk.Widget) #buttonReleaseEvent $ \handler btn -> do
    get btn #button >>= \case
      1 -> True <$ handler ()
      _ -> pure False
