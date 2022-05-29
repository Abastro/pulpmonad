module UI.X11.DesktopVisual.View (
  ImageSet (..),
  DeskVisualView,
  DeskVisualViewOp (..),
  deskVisualWidget,
  deskVisualItemAt,
  deskVisualViewNew,
  deskVisualViewCtrl,
  DeskItemView,
  DeskItemViewOp (..),
  deskItemWidget,
  deskItemViewNew,
  deskItemViewCtrl,
  WinItemView,
  winItemWidget,
  winItemViewNew,
  winItemViewSetImg,
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

-- | Setting image for Image widget
data ImageSet = ImgSName T.Text | ImgSGIcon Gio.Icon

-- | Desktop visualizer view
data DeskVisualView = DeskVisualView
  { deskVisualWidget :: !UI.Widget
  , deskVisualCont :: !UI.Container
  , deskVisualItems :: !(IORef (V.Vector DeskItemView))
  }

data DeskVisualViewOp
  = CutDeskItemsTo !Int
  | AddDeskItems !(V.Vector DeskItemView)

deskVisualWidget :: DeskVisualView -> UI.Widget
deskVisualWidget DeskVisualView{deskVisualWidget} = deskVisualWidget

deskVisualItemAt :: MonadIO m => DeskVisualView -> Int -> m (Maybe DeskItemView)
deskVisualItemAt DeskVisualView{..} idx = do
  deskItems <- liftIO $ readIORef deskVisualItems
  pure $ deskItems V.!? idx

deskVisualViewNew :: MonadIO m => m DeskVisualView
deskVisualViewNew = do
  deskVisualCont <- UI.toContainer =<< UI.boxNew UI.OrientationHorizontal 5
  deskVisualWidget <- UI.toWidget deskVisualCont

  deskVisualItems <- liftIO $ newIORef V.empty
  pure DeskVisualView{..}

deskVisualViewCtrl :: MonadIO m => DeskVisualView -> DeskVisualViewOp -> m ()
deskVisualViewCtrl DeskVisualView{deskVisualCont} = \case
  CutDeskItemsTo newCnt -> do
    toDrop <- drop newCnt <$> UI.containerGetChildren deskVisualCont
    for_ toDrop $ \deskItem -> UI.containerRemove deskVisualCont deskItem
  AddDeskItems deskItems -> do
    for_ deskItems $ \DeskItemView{deskItemWidget} -> do
      UI.containerAdd deskVisualCont deskItemWidget

-- | Desktop item view
data DeskItemView = DeskItemView
  { deskItemWidget :: !UI.Widget
  -- ^ The congregated widget
  , deskItemName :: !UI.Label
  , deskItemWinCont :: !UI.Container
  }

data DeskItemViewOp
  = FlushWinItems
  | AddWinItems !(V.Vector WinItemView)
  | DeskLabelName !T.Text

deskItemWidget :: DeskItemView -> UI.Widget
deskItemWidget DeskItemView{deskItemWidget} = deskItemWidget

deskItemViewNew :: MonadIO m => IO () -> m DeskItemView
deskItemViewNew onClick = do
  deskItemWinCont <- UI.toContainer =<< UI.boxNew UI.OrientationHorizontal 0

  deskItemName <- UI.labelNew Nothing
  UI.widgetGetStyleContext deskItemName >>= flip UI.styleContextAddClass (T.pack "desktop-label")

  deskMain <-
    UI.boxed UI.OrientationHorizontal 0
      =<< sequenceA [UI.toWidget deskItemName, UI.toWidget deskItemWinCont]

  deskItemWidget <- UI.buttonNewWith (Just deskMain) onClick
  UI.widgetGetStyleContext deskItemWidget >>= flip UI.styleContextAddClass (T.pack "desktop-item")

  pure DeskItemView{..}

deskItemViewCtrl :: MonadIO m => DeskItemView -> DeskItemViewOp -> m ()
deskItemViewCtrl DeskItemView{..} = \case
  FlushWinItems ->
    UI.containerForeach deskItemWinCont $ \winItem ->
      UI.containerRemove deskItemWinCont winItem
  AddWinItems winItems -> do
    for_ winItems $ \WinItemView{winItemWidget} -> do
      UI.containerAdd deskItemWinCont winItemWidget
  DeskLabelName name -> UI.labelSetLabel deskItemName name

-- | Window item view
data WinItemView = WinItemView
  { winItemWidget :: !UI.Widget
  -- ^ The congregated widget
  , winItemImg :: !UI.Image
  }

winItemWidget :: WinItemView -> UI.Widget
winItemWidget WinItemView{winItemWidget} = winItemWidget

winItemViewNew :: MonadIO m => IO () -> m WinItemView
winItemViewNew onClick = do
  winItemImg <- UI.imageNew

  winImg <- UI.toWidget winItemImg
  winItemWidget <- UI.buttonNewWith (Just winImg) onClick
  UI.widgetGetStyleContext winItemWidget >>= flip UI.styleContextAddClass (T.pack "window-item")

  pure WinItemView{..}

winItemViewSetImg :: MonadIO m => WinItemView -> ImageSet -> m ()
winItemViewSetImg WinItemView{winItemImg} = \case
  ImgSName iconName -> UI.imageSetFromIconName winItemImg (Just iconName) size
  ImgSGIcon gIcon -> UI.imageSetFromGicon winItemImg gIcon size
  where
    size = fromIntegral $ fromEnum UI.IconSizeLargeToolbar
