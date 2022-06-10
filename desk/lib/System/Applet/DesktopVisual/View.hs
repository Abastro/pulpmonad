module System.Applet.DesktopVisual.View (
  widgetUpdateClass,
  DeskVisual,
  DeskVisualOp (..),
  deskVisualWidget,
  deskVisualItemAt,
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

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Default.Class
import Data.Foldable
import Data.IORef
import Data.Maybe
import Data.Text qualified as T
import Data.Vector qualified as V
import GI.Gtk.Objects.Box qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.Pixbufs qualified as Gtk
import Gtk.Styles qualified as Gtk
import View.Boxes qualified as View
import View.Imagery qualified as View
import View.Textual qualified as View
import qualified GI.Gio.Interfaces.Icon as Gio
import Control.Applicative

widgetUpdateClass :: (Enum s, Bounded s, MonadIO m) => Gtk.Widget -> (s -> T.Text) -> [s] -> m ()
widgetUpdateClass widget asClass state =
  Gtk.widgetGetStyleContext widget >>= Gtk.updateCssClass asClass state

-- | Desktop visualizer view
data DeskVisual = DeskVisual
  { deskVisualWid :: !Gtk.Widget
  , deskVisualCont :: !Gtk.Container
  , deskVisualItems :: !(IORef (V.Vector DeskItem))
  }

data DeskVisualOp
  = CutDeskItemsTo !Int
  | AddDeskItems !(V.Vector DeskItem)

deskVisualWidget :: DeskVisual -> Gtk.Widget
deskVisualWidget DeskVisual{deskVisualWid} = deskVisualWid

-- TODO Remove itemAt
{-# DEPRECATED deskVisualItemAt "TBDel" #-}
deskVisualItemAt :: MonadIO m => DeskVisual -> Int -> m (Maybe DeskItem)
deskVisualItemAt DeskVisual{..} idx = do
  deskItems <- liftIO $ readIORef deskVisualItems
  pure $ deskItems V.!? idx

deskVisualNew :: MonadIO m => m DeskVisual
deskVisualNew = do
  deskVisualCont <- Gtk.toContainer =<< Gtk.boxNew Gtk.OrientationHorizontal 5
  deskVisualWid <- Gtk.toWidget deskVisualCont
  Gtk.widgetGetStyleContext deskVisualWid >>= flip Gtk.styleContextAddClass (T.pack "desk-visual")
  Gtk.widgetShowAll deskVisualWid

  deskVisualItems <- liftIO $ newIORef V.empty
  pure DeskVisual{..}

deskVisualCtrl :: MonadIO m => DeskVisual -> DeskVisualOp -> m ()
deskVisualCtrl DeskVisual{..} = \case
  CutDeskItemsTo newCnt -> do
    toDelete <- liftIO $ atomicModifyIORef' deskVisualItems $ V.splitAt newCnt
    for_ toDelete $ \DeskItem{deskItemWid} -> do
      Gtk.widgetHide deskItemWid
      Gtk.widgetDestroy deskItemWid
  AddDeskItems deskItems -> do
    liftIO $ modifyIORef' deskVisualItems (<> deskItems)
    for_ deskItems $ \DeskItem{deskItemWid} -> do
      Gtk.containerAdd deskVisualCont deskItemWid
      Gtk.widgetShowAll deskItemWid

-- | Desktop item view
data DeskItem = DeskItem
  { -- | The congregated widget
    deskItemWid :: !Gtk.Widget
  , deskItemName :: !View.LabelDyn
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

  deskItemName <- View.labelDynNew def
  Gtk.widgetGetStyleContext (View.labelDynWidget deskItemName)
    >>= flip Gtk.styleContextAddClass (T.pack "desktop-label")

  deskMain <-
    View.boxStaticNew
      (View.defBoxArg Gtk.OrientationHorizontal)
      [View.labelDynWidget deskItemName, View.boxUniDynWidget deskItemWinCont]

  deskItemWid <- Gtk.clickyNewWith (Just deskMain) onClick
  Gtk.widgetGetStyleContext deskItemWid >>= flip Gtk.styleContextAddClass (T.pack "desktop-item")

  pure DeskItem{..}

deskItemCtrl :: MonadIO m => DeskItem -> DeskItemOp -> m ()
deskItemCtrl DeskItem{..} = \case
  AddWinItemAt WinItem{winItemWid} idx -> do
    View.boxUniDynCtrl deskItemWinCont (View.BoxUniAdd winItemWid)
    View.boxUniDynCtrl deskItemWinCont (View.BoxUniReorder winItemWid $ fromIntegral idx)
    Gtk.widgetShowAll winItemWid
  RemoveWinItem WinItem{winItemWid} -> do
    Gtk.widgetHide winItemWid
    View.boxUniDynCtrl deskItemWinCont (View.BoxUniRemove winItemWid)
  ReorderWinItems winItems -> do
    for_ (V.indexed winItems) $ \(idx, WinItem{winItemWid}) -> do
      View.boxUniDynCtrl deskItemWinCont (View.BoxUniReorder winItemWid $ fromIntegral idx)

  -- Mundane property update here
  DeskLabelName name -> View.labelDynSetLabel deskItemName name
  DeskVisibility flag -> if flag then Gtk.widgetShowAll deskItemWid else Gtk.widgetHide deskItemWid

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
  Gtk.widgetGetStyleContext winItemWid >>= flip Gtk.styleContextAddClass (T.pack "window-item")
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
    rawIconSet = liftIO rawIcons >>= \case
          Left err -> MaybeT $ Nothing <$ liftIO (putStrLn $ "Cannot recognize icon: " <> err)
          Right icons -> do
            scaled <- MaybeT $ Gtk.iconsChoosePixbuf (Gtk.iconSizePx winItemSize) Gtk.argbTorgba icons
            pure (View.ImgSPixbuf scaled)
