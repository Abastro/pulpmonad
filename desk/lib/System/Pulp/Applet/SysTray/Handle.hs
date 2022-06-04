module System.Pulp.Applet.SysTray.Handle where

import DBus.Client
import Data.IORef
import Data.Map.Strict qualified as M
import StatusNotifier.Host.Service
import System.Pulp.Applet.SysTray.View qualified as View
import UI.Commons qualified as UI
import UI.Task qualified as UI

data SysTrayArgs = SysTrayArgs
  { trayOrientation :: !UI.Orientation
  , trayIconSize :: !UI.IconSize
  , -- | Whether to align from the beginning
    trayAlignBegin :: !Bool
  }

data SysTrayHandle = SysTrayHandle

sysTrayMake :: Host -> Client -> SysTrayArgs -> View.SysTray -> IO SysTrayHandle
sysTrayMake Host{..} client SysTrayArgs{..} view = do
  registers =<< newIORef M.empty
  where
    registers itemsRef = do
      trayHandle <- addUpdateHandler $ \typ info -> UI.uiSingleRun (onUpdate info typ)
      UI.onWidgetDestroy (View.sysTrayWidget view) $ removeUpdateHandler trayHandle
      pure SysTrayHandle
      where
        onUpdate :: ItemInfo -> UpdateType -> IO ()
        onUpdate info@ItemInfo{itemServiceName} typ = do
          items <- readIORef itemsRef
          items' <- M.alterF (alterItem info typ) itemServiceName items
          writeIORef itemsRef items'

        alterItem :: ItemInfo -> UpdateType -> Maybe TrayItemHandle -> IO (Maybe TrayItemHandle)
        alterItem info@ItemInfo{itemToolTip} = \case
          ItemAdded -> \case
            Just _ -> error "warn user: item exist"
            Nothing -> do
              itemView <- View.trayItemNew
              item <- trayItemMake itemView
              -- TODO Add corresponding view
              pure (Just item)
          ItemRemoved -> \case
            Nothing -> error "warn user: item not exist"
            Just _item -> do
              -- TODO Remove corresponding view
              pure Nothing

          -- Icon updates. TODO Add on update?
          IconUpdated -> maybe undefined $ \_item -> undefined
          OverlayIconUpdated -> maybe undefined $ \_item -> undefined
          -- Tooltip updates
          ToolTipUpdated -> traverse $ \item@TrayItemHandle{itemUpdateTooltip} ->
            item <$ itemUpdateTooltip itemToolTip
          -- Not handled
          StatusUpdated -> pure
          TitleUpdated -> pure

data TrayItemHandle = TrayItemHandle
  { itemUpdateIcon :: IO ()
  , itemUpdateTooltip :: Maybe (String, ImageInfo, String, String) -> IO ()
  }

trayItemMake :: View.TrayItem -> IO TrayItemHandle
trayItemMake view = do
  undefined
