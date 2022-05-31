module System.Pulp.Applet.DesktopVisual.View (
  ImageSet (..),
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
  winItemSetImg,
) where

import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.Text qualified as T
import Data.Vector qualified as V
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gtk.Objects.Box qualified as UI
import GI.Gtk.Objects.Image qualified as UI
import UI.Commons qualified as UI
import UI.Containers qualified as UI
import UI.Singles qualified as UI
import UI.Styles qualified as UI
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk

-- | Setting image for Image widget
data ImageSet = ImgSName T.Text | ImgSGIcon Gio.Icon | ImgSPixbuf Gdk.Pixbuf

widgetUpdateClass :: (Enum s, Bounded s, MonadIO m) => UI.Widget -> (s -> T.Text) -> [s] -> m ()
widgetUpdateClass widget asClass state =
  UI.widgetGetStyleContext widget >>= UI.updateCssClass asClass state

-- | Desktop visualizer view
data DeskVisual = DeskVisual
  { deskVisualWidget :: !UI.Widget
  , deskVisualCont :: !UI.Container
  , deskVisualItems :: !(IORef (V.Vector DeskItem))
  }

data DeskVisualOp
  = CutDeskItemsTo !Int
  | AddDeskItems !(V.Vector DeskItem)

deskVisualWidget :: DeskVisual -> UI.Widget
deskVisualWidget DeskVisual{deskVisualWidget} = deskVisualWidget

deskVisualItemAt :: MonadIO m => DeskVisual -> Int -> m (Maybe DeskItem)
deskVisualItemAt DeskVisual{..} idx = do
  deskItems <- liftIO $ readIORef deskVisualItems
  pure $ deskItems V.!? idx

deskVisualNew :: MonadIO m => m DeskVisual
deskVisualNew = do
  deskVisualCont <- UI.toContainer =<< UI.boxNew UI.OrientationHorizontal 5
  deskVisualWidget <- UI.toWidget deskVisualCont
  UI.widgetGetStyleContext deskVisualWidget >>= flip UI.styleContextAddClass (T.pack "desk-visual")

  deskVisualItems <- liftIO $ newIORef V.empty
  pure DeskVisual{..}

deskVisualCtrl :: MonadIO m => DeskVisual -> DeskVisualOp -> m ()
deskVisualCtrl DeskVisual{..} = \case
  CutDeskItemsTo newCnt -> do
    toDelete <- liftIO $ atomicModifyIORef' deskVisualItems $ V.splitAt newCnt
    for_ toDelete $ \DeskItem{deskItemWidget} -> do
      UI.widgetHide deskItemWidget
      UI.widgetDestroy deskItemWidget
  AddDeskItems deskItems -> do
    liftIO $ modifyIORef' deskVisualItems (<> deskItems)
    for_ deskItems $ \DeskItem{deskItemWidget} -> do
      UI.containerAdd deskVisualCont deskItemWidget
      UI.widgetShowAll deskItemWidget

-- | Desktop item view
data DeskItem = DeskItem
  { deskItemWidget :: !UI.Widget
  -- ^ The congregated widget
  , deskItemName :: !UI.Label
  , deskItemWinCont :: !UI.Box
  }

data DeskItemOp
  = AddWinItemAt !WinItem !Int
  | RemoveWinItem !WinItem
  | ReorderWinItems !(V.Vector WinItem)
  | DeskLabelName !T.Text

deskItemWidget :: DeskItem -> UI.Widget
deskItemWidget DeskItem{deskItemWidget} = deskItemWidget

deskItemNew :: MonadIO m => IO () -> m DeskItem
deskItemNew onClick = do
  deskItemWinCont <- UI.boxNew UI.OrientationHorizontal 0

  deskItemName <- UI.labelNew Nothing
  UI.widgetGetStyleContext deskItemName >>= flip UI.styleContextAddClass (T.pack "desktop-label")

  deskMain <-
    UI.boxed UI.OrientationHorizontal 0
      =<< sequenceA [UI.toWidget deskItemName, UI.toWidget deskItemWinCont]

  deskItemWidget <- UI.buttonNewWith (Just deskMain) onClick
  UI.widgetGetStyleContext deskItemWidget >>= flip UI.styleContextAddClass (T.pack "desktop-item")

  pure DeskItem{..}

deskItemCtrl :: MonadIO m => DeskItem -> DeskItemOp -> m ()
deskItemCtrl DeskItem{..} = \case
  AddWinItemAt WinItem{winItemWidget} idx -> do
    UI.containerAdd deskItemWinCont winItemWidget
    UI.boxReorderChild deskItemWinCont winItemWidget $ fromIntegral idx
    UI.widgetShowAll winItemWidget
  RemoveWinItem WinItem{winItemWidget} -> do
    UI.widgetHide winItemWidget
    UI.containerRemove deskItemWinCont winItemWidget
  ReorderWinItems winItems -> do
    for_ (V.indexed winItems) $ \(idx, WinItem{winItemWidget}) -> do
      UI.boxReorderChild deskItemWinCont winItemWidget $ fromIntegral idx

  -- Mundane property update here
  DeskLabelName name -> UI.labelSetLabel deskItemName name

-- | Window item view
data WinItem = WinItem
  { winItemWidget :: !UI.Widget
  -- ^ The congregated widget
  , winItemImg :: !UI.Image
  }

winItemWidget :: WinItem -> UI.Widget
winItemWidget WinItem{winItemWidget} = winItemWidget

winItemNew :: MonadIO m => IO () -> m WinItem
winItemNew onClick = do
  winItemImg <- UI.imageNew

  winImg <- UI.toWidget winItemImg
  winItemWidget <- UI.buttonNewWith (Just winImg) onClick
  UI.widgetGetStyleContext winItemWidget >>= flip UI.styleContextAddClass (T.pack "window-item")

  pure WinItem{..}

winItemSetImg :: MonadIO m => WinItem -> ImageSet -> m ()
winItemSetImg WinItem{winItemImg} = \case
  ImgSName iconName -> UI.imageSetFromIconName winItemImg (Just iconName) size
  ImgSGIcon gIcon -> UI.imageSetFromGicon winItemImg gIcon size
  ImgSPixbuf pixbuf -> UI.imageSetFromPixbuf winItemImg (Just pixbuf)
  where
    size = fromIntegral $ fromEnum UI.IconSizeLargeToolbar
