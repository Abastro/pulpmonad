module System.Pulp.Applet.DesktopVisual.View (
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
import Data.Foldable
import Data.IORef
import Data.Maybe
import Data.Text qualified as T
import Data.Vector qualified as V
import GI.Gtk.Objects.Box qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.Pixbufs qualified as Gtk
import Gtk.Singles qualified as Gtk
import Gtk.Styles qualified as Gtk
import View.Imagery qualified as View

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
  , deskItemName :: !Gtk.Label
  , deskItemWinCont :: !Gtk.Box
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
  deskItemWinCont <- Gtk.boxNew Gtk.OrientationHorizontal 0

  deskItemName <- Gtk.labelNew Nothing
  Gtk.widgetGetStyleContext deskItemName >>= flip Gtk.styleContextAddClass (T.pack "desktop-label")

  deskMain <-
    Gtk.boxed Gtk.OrientationHorizontal 0
      =<< sequenceA [Gtk.toWidget deskItemName, Gtk.toWidget deskItemWinCont]

  deskItemWid <- Gtk.clickyNewWith (Just deskMain) onClick
  Gtk.widgetGetStyleContext deskItemWid >>= flip Gtk.styleContextAddClass (T.pack "desktop-item")

  pure DeskItem{..}

deskItemCtrl :: MonadIO m => DeskItem -> DeskItemOp -> m ()
deskItemCtrl DeskItem{..} = \case
  AddWinItemAt WinItem{winItemWid} idx -> do
    Gtk.containerAdd deskItemWinCont winItemWid
    Gtk.boxReorderChild deskItemWinCont winItemWid $ fromIntegral idx
    Gtk.widgetShowAll winItemWid
  RemoveWinItem WinItem{winItemWid} -> do
    Gtk.widgetHide winItemWid
    Gtk.containerRemove deskItemWinCont winItemWid
  ReorderWinItems winItems -> do
    for_ (V.indexed winItems) $ \(idx, WinItem{winItemWid}) -> do
      Gtk.boxReorderChild deskItemWinCont winItemWid $ fromIntegral idx

  -- Mundane property update here
  DeskLabelName name -> Gtk.labelSetLabel deskItemName name
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
  winItemImg <- View.imageDynNew winItemSize
  winItemWid <- Gtk.buttonNewWith (Just $ View.imageDynWidget winItemImg) onClick
  Gtk.widgetGetStyleContext winItemWid >>= flip Gtk.styleContextAddClass (T.pack "window-item")
  pure WinItem{..}

winItemSetTitle :: MonadIO m => WinItem -> T.Text -> m ()
winItemSetTitle WinItem{winItemWid} = Gtk.widgetSetTooltipText winItemWid . Just

type RawIconSet = IO (Either String [Gtk.RawIcon])

winItemSetIcon :: MonadIO m => WinItem -> RawIconSet -> Maybe View.ImageSet -> m ()
winItemSetIcon WinItem{..} rawIcons = \case
  Nothing -> do
    iconSet <- fromMaybe (View.ImgSName $ T.pack "missing") <$> liftIO rawIconSet
    View.imageDynSetImg winItemImg iconSet
  Just iconSet -> View.imageDynSetImg winItemImg iconSet
  where
    rawIconSet = runMaybeT $ liftIO rawIcons >>= \case
      Left err -> MaybeT $ Nothing <$ liftIO (putStrLn $ "Cannot recognize icon: " <> err)
      Right icons -> do
        scaled <- MaybeT $ Gtk.iconsChoosePixbuf (Gtk.iconSizePx winItemSize) Gtk.argbTorgba icons
        pure (View.ImgSPixbuf scaled)
