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
import GI.GdkPixbuf.Objects.Pixbuf qualified as Gdk
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gtk.Objects.Box qualified as UI
import GI.Gtk.Objects.Image qualified as UI
import UI.Commons qualified as UI
import UI.Containers qualified as UI
import UI.Singles qualified as UI
import UI.Styles qualified as UI

-- | Setting image for Image widget
data ImageSet = ImgSName T.Text | ImgSGIcon Gio.Icon | ImgSPixbuf Gdk.Pixbuf

widgetUpdateClass :: (Enum s, Bounded s, MonadIO m) => UI.Widget -> (s -> T.Text) -> [s] -> m ()
widgetUpdateClass widget asClass state =
  UI.widgetGetStyleContext widget >>= UI.updateCssClass asClass state

-- | Desktop visualizer view
data DeskVisual = DeskVisual
  { deskVisualWid :: !UI.Widget
  , deskVisualCont :: !UI.Container
  , deskVisualItems :: !(IORef (V.Vector DeskItem))
  }

data DeskVisualOp
  = CutDeskItemsTo !Int
  | AddDeskItems !(V.Vector DeskItem)

deskVisualWidget :: DeskVisual -> UI.Widget
deskVisualWidget DeskVisual{deskVisualWid} = deskVisualWid

-- TODO Remove itemAt
deskVisualItemAt :: MonadIO m => DeskVisual -> Int -> m (Maybe DeskItem)
deskVisualItemAt DeskVisual{..} idx = do
  deskItems <- liftIO $ readIORef deskVisualItems
  pure $ deskItems V.!? idx

deskVisualNew :: MonadIO m => m DeskVisual
deskVisualNew = do
  deskVisualCont <- UI.toContainer =<< UI.boxNew UI.OrientationHorizontal 5
  deskVisualWid <- UI.toWidget deskVisualCont
  UI.widgetGetStyleContext deskVisualWid >>= flip UI.styleContextAddClass (T.pack "desk-visual")
  UI.widgetShowAll deskVisualWid

  deskVisualItems <- liftIO $ newIORef V.empty
  pure DeskVisual{..}

deskVisualCtrl :: MonadIO m => DeskVisual -> DeskVisualOp -> m ()
deskVisualCtrl DeskVisual{..} = \case
  CutDeskItemsTo newCnt -> do
    toDelete <- liftIO $ atomicModifyIORef' deskVisualItems $ V.splitAt newCnt
    for_ toDelete $ \DeskItem{deskItemWid} -> do
      UI.widgetHide deskItemWid
      UI.widgetDestroy deskItemWid
  AddDeskItems deskItems -> do
    liftIO $ modifyIORef' deskVisualItems (<> deskItems)
    for_ deskItems $ \DeskItem{deskItemWid} -> do
      UI.containerAdd deskVisualCont deskItemWid
      UI.widgetShowAll deskItemWid

-- | Desktop item view
data DeskItem = DeskItem
  { -- | The congregated widget
    deskItemWid :: !UI.Widget
  , deskItemName :: !UI.Label
  , deskItemWinCont :: !UI.Box
  }

data DeskItemOp
  = AddWinItemAt !WinItem !Int
  | RemoveWinItem !WinItem
  | ReorderWinItems !(V.Vector WinItem)
  | DeskLabelName !T.Text
  | DeskVisibility !Bool

deskItemWidget :: DeskItem -> UI.Widget
deskItemWidget DeskItem{deskItemWid} = deskItemWid

deskItemNew :: MonadIO m => IO () -> m DeskItem
deskItemNew onClick = do
  deskItemWinCont <- UI.boxNew UI.OrientationHorizontal 0

  deskItemName <- UI.labelNew Nothing
  UI.widgetGetStyleContext deskItemName >>= flip UI.styleContextAddClass (T.pack "desktop-label")

  deskMain <-
    UI.boxed UI.OrientationHorizontal 0
      =<< sequenceA [UI.toWidget deskItemName, UI.toWidget deskItemWinCont]

  deskItemWid <- UI.clickyNewWith (Just deskMain) onClick
  UI.widgetGetStyleContext deskItemWid >>= flip UI.styleContextAddClass (T.pack "desktop-item")

  pure DeskItem{..}

deskItemCtrl :: MonadIO m => DeskItem -> DeskItemOp -> m ()
deskItemCtrl DeskItem{..} = \case
  AddWinItemAt WinItem{winItemWid} idx -> do
    UI.containerAdd deskItemWinCont winItemWid
    UI.boxReorderChild deskItemWinCont winItemWid $ fromIntegral idx
    UI.widgetShowAll winItemWid
  RemoveWinItem WinItem{winItemWid} -> do
    UI.widgetHide winItemWid
    UI.containerRemove deskItemWinCont winItemWid
  ReorderWinItems winItems -> do
    for_ (V.indexed winItems) $ \(idx, WinItem{winItemWid}) -> do
      UI.boxReorderChild deskItemWinCont winItemWid $ fromIntegral idx

  -- Mundane property update here
  DeskLabelName name -> UI.labelSetLabel deskItemName name
  DeskVisibility flag -> if flag then UI.widgetShowAll deskItemWid else UI.widgetHide deskItemWid

-- | Window item view
data WinItem = WinItem
  { -- | The congregated widget
    winItemWid :: !UI.Widget
  , winItemImg :: !UI.Image
  }

winItemWidget :: WinItem -> UI.Widget
winItemWidget WinItem{winItemWid} = winItemWid

winItemNew :: MonadIO m => IO () -> m WinItem
winItemNew onClick = do
  winItemImg <- UI.imageNew

  winImg <- UI.toWidget winItemImg
  winItemWid <- UI.buttonNewWith (Just winImg) onClick
  UI.widgetGetStyleContext winItemWid >>= flip UI.styleContextAddClass (T.pack "window-item")

  pure WinItem{..}

winItemSetImg :: MonadIO m => WinItem -> ImageSet -> m ()
winItemSetImg WinItem{winItemImg} = \case
  ImgSName iconName -> UI.imageSetFromIconName winItemImg (Just iconName) size
  ImgSGIcon gIcon -> UI.imageSetFromGicon winItemImg gIcon size
  ImgSPixbuf pixbuf -> UI.imageSetFromPixbuf winItemImg (Just pixbuf)
  where
    size = fromIntegral $ fromEnum UI.IconSizeLargeToolbar
