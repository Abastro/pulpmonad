module System.Pulp.Applet.SysTray.View (
  SysTray,
  SysTrayOp (..),
  sysTrayWidget,
  sysTrayNew,
  sysTrayCtrl,
  TrayItem,
  TrayItemOp (..),
  TrayItemInput (..),
  MouseButton (..),
  trayItemWidget,
  trayItemNew,
  trayItemCtrl,
) where

import Control.Monad.IO.Class
import Data.Int
import Data.Text qualified as T
import GI.DbusmenuGtk3.Objects.Menu qualified as DBus
import GI.Gdk.Flags qualified as Gdk
import GI.Gdk.Structs.EventButton qualified as Gdk
import GI.Gdk.Structs.EventScroll qualified as Gdk
import GI.Gtk.Objects.Box qualified as UI
import GI.Gtk.Objects.EventBox qualified as UI
import GI.Gtk.Objects.Menu qualified as UI
import UI.Commons qualified as UI
import UI.Containers qualified as UI
import UI.Styles qualified as UI
import View.Imagery qualified as View

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
  UI.widgetGetStyleContext sysTrayWid >>= flip UI.styleContextAddClass (T.pack "system-tray-area")
  pure SysTray{..}

sysTrayCtrl :: MonadIO m => SysTray -> SysTrayOp -> m ()
sysTrayCtrl SysTray{..} = \case
  TrayAddItem TrayItem{trayItemWid} -> do
    boxPack sysTrayBox trayItemWid False False 0
    UI.widgetShowAll trayItemWid
  TrayRemoveItem TrayItem{trayItemWid} -> do
    UI.widgetHide trayItemWid
    UI.widgetDestroy trayItemWid
  where
    boxPack = if sysTrayAlignBegin then UI.boxPackStart else UI.boxPackEnd

data TrayItem = TrayItem
  { trayItemWid :: !UI.Widget
  , trayItemIcon :: !View.ImageDyn
  , trayItemOverlay :: !View.ImageDyn
  }

data TrayItemOp
  = ItemSetInputHandler !(TrayItemInput -> IO ())
  | ItemSetIcon !View.ImageSet
  | ItemSetOverlay !View.ImageSet
  | ItemSetTooltip !(Maybe T.Text)
  | ItemShowPopup !DBus.Menu

data MouseButton = MouseLeft | MouseMiddle | MouseRight
data TrayItemInput
  = TrayItemScroll !UI.ScrollDirection
  | TrayItemClick !MouseButton !Int32 !Int32

trayItemWidget :: TrayItem -> UI.Widget
trayItemWidget TrayItem{trayItemWid} = trayItemWid

trayItemNew :: MonadIO m => UI.IconSize -> m TrayItem
trayItemNew size = do
  trayItemIcon <- View.imageDynNew size
  trayItemOverlay <- View.imageDynNew size
  overlay <- UI.overlayed (View.imageDynWidget trayItemIcon) [View.imageDynWidget trayItemOverlay]

  interactive <- UI.eventBoxNew
  UI.widgetAddEvents interactive [Gdk.EventMaskScrollMask]
  UI.containerAdd interactive overlay
  trayItemWid <- UI.toWidget interactive
  UI.widgetGetStyleContext trayItemWid >>= flip UI.styleContextAddClass (T.pack "system-tray-item")

  pure TrayItem{..}

trayItemCtrl :: MonadIO m => TrayItem -> TrayItemOp -> m ()
trayItemCtrl TrayItem{..} = \case
  ItemSetTooltip tooltip -> UI.widgetSetTooltipText trayItemWid tooltip
  ItemSetIcon setIcon -> View.imageDynSetImg trayItemIcon setIcon
  ItemSetOverlay setIcon -> View.imageDynSetImg trayItemOverlay setIcon
  ItemSetInputHandler handler -> do
    UI.onWidgetButtonPressEvent trayItemWid $ \event -> do
      xRoot <- round <$> Gdk.getEventButtonXRoot event
      yRoot <- round <$> Gdk.getEventButtonYRoot event
      Gdk.getEventButtonButton event >>= \case
        1 -> True <$ handler (TrayItemClick MouseLeft xRoot yRoot)
        2 -> True <$ handler (TrayItemClick MouseMiddle xRoot yRoot)
        3 -> True <$ handler (TrayItemClick MouseRight xRoot yRoot)
        _ -> pure False
    UI.onWidgetScrollEvent trayItemWid $ \event -> do
      direction <- Gdk.getEventScrollDirection event
      True <$ handler (TrayItemScroll direction)
    pure ()
  ItemShowPopup menu -> do
    UI.menuPopupAtWidget menu trayItemWid UI.GravitySouthWest UI.GravityNorthWest Nothing
