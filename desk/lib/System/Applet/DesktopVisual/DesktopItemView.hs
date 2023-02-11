{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Applet.DesktopVisual.DesktopItemView (
  DesktopItemView (..),
  insertWindow,
  removeWindow,
  reflectPriority,
  desktopSetLabel,
  desktopSetVisible,
  DesktopState (..),
  desktopSetState,
  desktopClickSource,
) where

import Control.Event.Entry
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
import Gtk.Commons qualified as Gtk
import Gtk.Reactive qualified as Gtk
import Gtk.Styles qualified as Gtk
import System.Applet.DesktopVisual.WindowItemView
import System.Pulp.PulpPath
import Status.X11.WMStatus

-- Desktop item which has windows sorted via priority
newtype DesktopItemView = DesktopItemView (ManagedPtr DesktopItemView)

instance TypedObject DesktopItemView where
  glibType :: IO GType
  glibType = registerGType DesktopItemView
instance GObject DesktopItemView

type instance ParentTypes DesktopItemView = Gtk.EventBox ': ParentTypes Gtk.EventBox
instance HasParentTypes DesktopItemView

-- Disallow box/container methods
instance
  ( info ~ Gtk.ResolveWidgetMethod t DesktopItemView
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
  type GObjectParentType DesktopItemView = Gtk.EventBox
  type GObjectPrivateData DesktopItemView = DesktopItemPrivate

  objectTypeName :: T.Text
  objectTypeName = T.pack "DesktopItemView"

  objectClassInit :: GObjectClass -> IO ()
  objectClassInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
    uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "desktop-visualizer" </> "desktop-item.ui")
    Gtk.setTemplateFromGFile widgetClass uiFile

    #bindTemplateChildFull widgetClass (T.pack "desktop-label") True 0
    #bindTemplateChildFull widgetClass (T.pack "desktop-container") True 0

    #setCssName widgetClass (T.pack "desktopitem")

  objectInstanceInit :: GObjectClass -> DesktopItemView -> IO DesktopItemPrivate
  objectInstanceInit _gClass inst = do
    #initTemplate inst
    desktopLabel <- Gtk.templateChild inst (T.pack "desktop-label") Gtk.Label
    desktopContainer <- Gtk.templateChild inst (T.pack "desktop-container") Gtk.FlowBox

    -- Sort function is pre-set
    #setSortFunc desktopContainer (Just sortFunc)

    pure DesktopItemPrivate{..}

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
      win <- unsafeCastTo WindowItemView wid
      getPriority win

insertWindow :: DesktopItemView -> WindowItemView -> IO ()
insertWindow desktop window = do
  DesktopItemPrivate{desktopContainer} <- gobjectGetPrivateData desktop
  -- FlowBox requires FlowBoxChild
  winChild <- new Gtk.FlowBoxChild [#child := window]
  #add desktopContainer winChild

removeWindow :: DesktopItemView -> WindowItemView -> IO ()
removeWindow desktop window = do
  DesktopItemPrivate{desktopContainer} <- gobjectGetPrivateData desktop
  -- Gets the parent, FlowBoxChild
  Just winChild <- traverse (unsafeCastTo Gtk.FlowBoxChild) =<< #getParent window
  -- And remove both parent-child deps
  #remove winChild window
  #remove desktopContainer winChild

-- | Reflect the priorities to the sort as priorities change.
--
-- This is here since in the end, priorities are usually updated in batch.
-- (Also, notifying FlowBoxChild can introduce coupling)
reflectPriority :: DesktopItemView -> IO ()
reflectPriority desktop = do
  DesktopItemPrivate{desktopContainer} <- gobjectGetPrivateData desktop
  #invalidateSort desktopContainer

desktopSetLabel :: DesktopItemView -> T.Text -> IO ()
desktopSetLabel desktop txt = do
  DesktopItemPrivate{desktopLabel} <- gobjectGetPrivateData desktop
  set desktopLabel [#label := txt]

desktopSetVisible :: DesktopItemView -> Bool -> IO ()
desktopSetVisible desktop = \case
  True -> #showAll desktop
  False -> #hide desktop

desktopSetState :: DesktopItemView -> DesktopState -> IO ()
desktopSetState desktop state = do
  #getStyleContext desktop >>= Gtk.updateCssClass asClass [state]
  where
    asClass = \case
      DeskActive -> T.pack "active"
      DeskVisible -> T.pack "visible"
      DeskHidden -> T.pack "hidden"

desktopClickSource :: DesktopItemView -> Source ()
desktopClickSource desktop =
  Gtk.onSource (desktop `asA` Gtk.Widget) #buttonReleaseEvent $ \handler btn -> do
    get btn #button >>= \case
      1 -> True <$ handler ()
      _ -> pure False
