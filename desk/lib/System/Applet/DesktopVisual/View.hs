{-# LANGUAGE OverloadedLabels #-}

module System.Applet.DesktopVisual.View (
  widgetUpdateClass,
  DeskVisual,
  DeskVisualOp (..),
  deskVisualWidget,
  deskVisualNew,
  deskVisualCtrl,
  DeskItem,
  DeskItemOp (..),
  deskItemWidget,
  deskItemNew,
  deskItemCtrl,
  WinItem,
  winItemWidget,
  winItemNew,
  winItemSetTitle,
  winItemSetIcon,
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.GI.Base.Constructible
import Data.IORef
import Data.Maybe
import Data.Text qualified as T
import Data.Vector qualified as V
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gtk.Objects.Box qualified as Gtk
import GI.Gtk.Objects.Label qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.Pixbufs qualified as Gtk
import Gtk.Styles qualified as Gtk
import View.Boxes qualified as View
import View.Imagery qualified as View
import Data.GI.Base.Attributes

widgetUpdateClass :: (Enum s, Bounded s, MonadIO m) => Gtk.Widget -> (s -> T.Text) -> [s] -> m ()
widgetUpdateClass widget asClass state =
  #getStyleContext widget >>= Gtk.updateCssClass asClass state

-- | Desktop visualizer view
data DeskVisual = DeskVisual
  { deskVisualWid :: !Gtk.Widget
  , -- Gtk Box, because we have unique actions
    deskVisualBox :: !Gtk.Box
  , deskVisualItems :: !(IORef (V.Vector DeskItem))
  }

data DeskVisualOp
  = CutDeskItemsTo !Int
  | AddDeskItems !(V.Vector DeskItem)

deskVisualWidget :: DeskVisual -> Gtk.Widget
deskVisualWidget DeskVisual{deskVisualWid} = deskVisualWid

deskVisualNew :: MonadIO m => m DeskVisual
deskVisualNew = do
  deskVisualBox <- Gtk.boxNew Gtk.OrientationHorizontal 5
  deskVisualWid <- Gtk.toWidget deskVisualBox
  #getStyleContext deskVisualWid >>= flip #addClass (T.pack "desk-visual")
  #showAll deskVisualWid

  deskVisualItems <- liftIO $ newIORef V.empty
  pure DeskVisual{..}

deskVisualCtrl :: MonadIO m => DeskVisual -> DeskVisualOp -> m ()
deskVisualCtrl DeskVisual{..} = \case
  CutDeskItemsTo newCnt -> do
    toDelete <- liftIO $ atomicModifyIORef' deskVisualItems $ V.splitAt newCnt
    for_ toDelete $ \DeskItem{deskItemWid} -> do
      #hide deskItemWid
      #remove deskVisualBox deskItemWid
  AddDeskItems deskItems -> do
    liftIO $ modifyIORef' deskVisualItems (<> deskItems)
    for_ deskItems $ \DeskItem{deskItemWid} -> do
      #add deskVisualBox deskItemWid
      #showAll deskItemWid

-- | Desktop item view
data DeskItem = DeskItem
  { -- | The congregated widget
    deskItemWid :: !Gtk.Widget
  , deskItemName :: !Gtk.Label
  , deskItemWinCont :: !View.BoxUniDyn
  }

data DeskItemOp
  = AddWinItemAt !WinItem !Int
  | RemoveWinItem !WinItem
  | ReorderWinItems !(V.Vector WinItem)
  | DeskLabelName !T.Text
  | DeskVisibility !Bool

deskItemWidget :: DeskItem -> Gtk.Widget
deskItemWidget DeskItem{deskItemWid} = deskItemWid

deskItemNew :: MonadIO m => IO () -> m DeskItem
deskItemNew onClick = do
  deskItemWinCont <- View.boxUniDynNew (View.defBoxArg Gtk.OrientationHorizontal)

  deskItemName <- new Gtk.Label []
  #getStyleContext deskItemName >>= flip #addClass (T.pack "desktop-label")
  deskItemNameWid <- Gtk.toWidget deskItemName

  deskMain <-
    View.boxStaticNew
      (View.defBoxArg Gtk.OrientationHorizontal)
      [deskItemNameWid, View.boxUniDynWidget deskItemWinCont]

  deskItemWid <- Gtk.clickyNewWith (Just deskMain) onClick
  #getStyleContext deskItemWid >>= flip #addClass (T.pack "desktop-item")

  pure DeskItem{..}

deskItemCtrl :: MonadIO m => DeskItem -> DeskItemOp -> m ()
deskItemCtrl DeskItem{..} = \case
  AddWinItemAt WinItem{winItemWid} idx -> do
    View.boxUniDynCtrl deskItemWinCont (View.BoxUniAdd winItemWid)
    View.boxUniDynCtrl deskItemWinCont (View.BoxUniReorder winItemWid $ fromIntegral idx)
    #showAll winItemWid
  RemoveWinItem WinItem{winItemWid} -> do
    #hide winItemWid
    View.boxUniDynCtrl deskItemWinCont (View.BoxUniRemove winItemWid)
  ReorderWinItems winItems -> do
    for_ (V.indexed winItems) $ \(idx, WinItem{winItemWid}) -> do
      View.boxUniDynCtrl deskItemWinCont (View.BoxUniReorder winItemWid $ fromIntegral idx)

  -- Mundane property update here
  DeskLabelName name -> set deskItemName [#label := name]
  DeskVisibility flag -> if flag then #showAll deskItemWid else #hide deskItemWid

-- | Window item view
data WinItem = WinItem
  { -- | The congregated widget
    winItemWid :: !Gtk.Widget
  , winItemImg :: !View.ImageDyn
  , winItemSize :: !Gtk.IconSize
  }

winItemWidget :: WinItem -> Gtk.Widget
winItemWidget WinItem{winItemWid} = winItemWid

winItemNew :: MonadIO m => Gtk.IconSize -> IO () -> m WinItem
winItemNew winItemSize onClick = do
  winItemImg <- View.imageDynNew winItemSize True
  winItemWid <- Gtk.buttonNewWith (Just $ View.imageDynWidget winItemImg) onClick
  #getStyleContext winItemWid >>= flip #addClass (T.pack "window-item")
  pure WinItem{..}

winItemSetTitle :: MonadIO m => WinItem -> T.Text -> m ()
winItemSetTitle WinItem{winItemWid} = Gtk.widgetSetTooltipText winItemWid . Just

type RawIconSet = IO (Either String [Gtk.RawIcon])

winItemSetIcon :: MonadIO m => WinItem -> RawIconSet -> Maybe Gio.Icon -> m ()
winItemSetIcon WinItem{..} rawIcons mayIcon = do
  iconSet <- runMaybeT $ giconSet <|> rawIconSet
  View.imageDynSetImg winItemImg $ fromMaybe (View.ImgSName $ T.pack "missing") iconSet
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
          scaled <- MaybeT $ Gtk.iconsChoosePixbuf (Gtk.iconSizePx winItemSize) Gtk.argbTorgba icons
          pure (View.ImgSPixbuf scaled)
