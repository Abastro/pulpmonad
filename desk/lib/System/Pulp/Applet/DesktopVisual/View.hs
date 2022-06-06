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
import GI.Gtk.Objects.Box qualified as UI
import UI.Commons qualified as UI
import UI.Containers qualified as UI
import UI.Pixbufs qualified as UI
import UI.Singles qualified as UI
import UI.Styles qualified as UI
import View.Imagery qualified as View

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
  , winItemImg :: !View.ImageDyn
  , winItemSize :: !UI.IconSize
  }

winItemWidget :: WinItem -> UI.Widget
winItemWidget WinItem{winItemWid} = winItemWid

winItemNew :: MonadIO m => UI.IconSize -> IO () -> m WinItem
winItemNew winItemSize onClick = do
  winItemImg <- View.imageDynNew winItemSize
  winItemWid <- UI.buttonNewWith (Just $ View.imageDynWidget winItemImg) onClick
  UI.widgetGetStyleContext winItemWid >>= flip UI.styleContextAddClass (T.pack "window-item")
  pure WinItem{..}

winItemSetTitle :: MonadIO m => WinItem -> T.Text -> m ()
winItemSetTitle WinItem{winItemWid} = UI.widgetSetTooltipText winItemWid . Just

type RawIconSet = IO (Either String [UI.RawIcon])

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
        scaled <- MaybeT $ UI.iconsChoosePixbuf (UI.iconSizePx winItemSize) UI.argbTorgba icons
        pure (View.ImgSPixbuf scaled)
