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
  winItemSetIcon,
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.IORef
import Data.Maybe
import Data.Text qualified as T
import Data.Vector qualified as V
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gtk.Objects.Box qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Pixbufs qualified as Gtk
import Gtk.Styles qualified as Gtk
import System.Applet.DesktopVisual.DesktopItemView
import System.Applet.DesktopVisual.WindowItemView
import View.Imagery qualified as View

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

type RawIconSet = IO (Either String [Gtk.RawIcon])

-- TODO Simplify this - Could be in WindowItemView
winItemSetIcon :: MonadIO m => WindowItemView -> RawIconSet -> Maybe Gio.Icon -> m ()
winItemSetIcon view rawIcons mayIcon = do
  iconSet <- runMaybeT $ giconSet <|> rawIconSet
  liftIO $ windowSetIcon view $ fromMaybe (View.ImgSName $ T.pack "missing") iconSet
  where
    giconSet = do
      icon <- MaybeT (pure mayIcon)
      pure (View.ImgSGIcon icon)

    -- NOTE Pixbuf has sharpness loss on scaling.
    -- MAYBE Make this go through LoadableIcon?
    rawIconSet =
      liftIO rawIcons >>= \case
        Left err -> MaybeT $ Nothing <$ liftIO (putStrLn $ "Cannot recognize icon: " <> err)
        Right icons -> do
          scaled <- MaybeT $ Gtk.iconsChoosePixbuf (Gtk.iconSizePx Gtk.IconSizeLargeToolbar) Gtk.argbTorgba icons
          pure (View.ImgSPixbuf scaled)
