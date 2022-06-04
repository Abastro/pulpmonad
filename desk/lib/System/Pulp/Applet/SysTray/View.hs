module System.Pulp.Applet.SysTray.View (
  SysTray,
  SysTrayOp,
  sysTrayWidget,
  sysTrayNew,
  sysTrayCtrl,
  TrayItem,
  TrayItemOp,
  trayItemWidget,
  trayItemNew,
  trayItemCtrl,
) where

import Control.Monad.IO.Class
import Data.Text qualified as T
import GI.Gdk.Flags qualified as Gdk
import GI.Gtk.Objects.Box qualified as UI
import GI.Gtk.Objects.EventBox qualified as UI
import GI.Gtk.Objects.Image qualified as UI
import UI.Commons qualified as UI
import UI.Containers qualified as UI

data SysTray = SysTray
  { sysTrayWid :: !UI.Widget
  , sysTrayBox :: !UI.Box
  , sysTrayAlignBegin :: !Bool
  }

data SysTrayOp = TrayAddItem !TrayItem | TrayRemoveItem !TrayItem

sysTrayWidget :: SysTray -> UI.Widget
sysTrayWidget SysTray{sysTrayWid} = sysTrayWid

sysTrayNew :: MonadIO m => UI.Orientation -> Bool -> m SysTray
sysTrayNew orientation sysTrayAlignBegin = do
  sysTrayBox <- UI.boxNew orientation 0
  sysTrayWid <- UI.toWidget sysTrayBox
  pure SysTray{..}

sysTrayCtrl :: MonadIO m => SysTray -> SysTrayOp -> m ()
sysTrayCtrl SysTray{..} = \case
  TrayAddItem TrayItem{trayItemWid}
    | boxPack <- if sysTrayAlignBegin then UI.boxPackStart else UI.boxPackEnd ->
      boxPack sysTrayBox trayItemWid False False 0
  TrayRemoveItem TrayItem{trayItemWid} -> UI.widgetDestroy trayItemWid

data TrayItem = TrayItem
  { trayItemWid :: !UI.Widget
  , trayItemIcon :: !UI.Image
  }

data TrayItemOp = ItemSetTooltip !T.Text

trayItemWidget :: TrayItem -> UI.Widget
trayItemWidget TrayItem{trayItemWid} = trayItemWid

trayItemNew :: MonadIO m => m TrayItem
trayItemNew = do
  trayItemIcon <- UI.imageNew

  interactive <- UI.eventBoxNew
  UI.widgetAddEvents interactive [Gdk.EventMaskScrollMask]
  UI.containerAdd interactive trayItemIcon
  trayItemWid <- UI.toWidget interactive

  pure TrayItem{..}

trayItemCtrl :: MonadIO m => TrayItem -> TrayItemOp -> m ()
trayItemCtrl TrayItem{..} = \case
  ItemSetTooltip tooltip -> UI.widgetSetTooltipText trayItemWid (Just tooltip)
