{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}

module System.Applet.DesktopVisual.View (
  widgetUpdateClass,
  DeskVisual,
  DeskVisualOp (..),
  deskVisualWidget,
  deskVisualNew,
  deskVisualCtrl,
  module System.Applet.DesktopVisual.DesktopItemView,
  module System.Applet.DesktopVisual.WindowItemView,
) where

import Control.Monad.IO.Class
import Data.Foldable
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.IORef
import Data.Text qualified as T
import Data.Vector qualified as V
import GI.Gtk.Objects.Box qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Styles qualified as Gtk
import System.Applet.DesktopVisual.DesktopItemView
import System.Applet.DesktopVisual.WindowItemView

widgetUpdateClass :: (Gtk.IsWidget w, Enum s, Bounded s, MonadIO m) => w -> (s -> T.Text) -> [s] -> m ()
widgetUpdateClass widget asClass state =
  Gtk.widgetGetStyleContext widget >>= Gtk.updateCssClass asClass state

-- | Desktop visualizer view
data DeskVisual = DeskVisual
  { deskVisualWid :: !Gtk.Widget
  , -- Gtk Box, because we have unique actions
    deskVisualBox :: !Gtk.Box
  , deskVisualItems :: !(IORef (V.Vector DesktopItemView))
  }

data DeskVisualOp
  = CutDeskItemsTo !Int
  | AddDeskItems !(V.Vector DesktopItemView)

deskVisualWidget :: DeskVisual -> Gtk.Widget
deskVisualWidget DeskVisual{deskVisualWid} = deskVisualWid

deskVisualNew :: MonadIO m => m DeskVisual
deskVisualNew = do
  deskVisualBox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 5]
  deskVisualWid <- Gtk.toWidget deskVisualBox
  #setName deskVisualWid (T.pack "desktop-visual")

  deskVisualItems <- liftIO $ newIORef V.empty
  pure DeskVisual{..}

-- TODO Phase this out
deskVisualCtrl :: MonadIO m => DeskVisual -> DeskVisualOp -> m ()
deskVisualCtrl DeskVisual{..} = \case
  CutDeskItemsTo newCnt -> do
    toDelete <- liftIO $ atomicModifyIORef' deskVisualItems $ V.splitAt newCnt
    for_ toDelete $ \desk -> do
      #hide desk
      #remove deskVisualBox desk
  AddDeskItems deskItems -> do
    liftIO $ modifyIORef' deskVisualItems (<> deskItems)
    for_ deskItems $ \desk -> do
      #add deskVisualBox desk
      #showAll desk
