{-# LANGUAGE OverloadedLabels #-}

module System.Applet.SysTray.View (
  SysTray,
  SysTrayOp (..),
  sysTrayWidget,
  sysTrayNew,
  sysTrayCtrl,
  TrayItemInput (..),
  MouseButton (..),
  TrayItemIcon (..),
  module System.Applet.SysTray.TrayItemView,
) where

import Control.Monad.IO.Class
import Data.Text qualified as T
import Gtk.Commons qualified as Gtk
import System.Applet.SysTray.TrayItemView
import View.Boxes qualified as View
import Data.GI.Base.Overloading

newtype SysTray = SysTray {sysTrayBox :: View.BoxUniDyn}

data SysTrayOp = TrayAddItem !TrayItemView | TrayRemoveItem !TrayItemView

sysTrayWidget :: SysTray -> Gtk.Widget
sysTrayWidget SysTray{sysTrayBox} = View.boxUniDynWidget sysTrayBox

sysTrayNew :: MonadIO m => Gtk.Orientation -> Bool -> m SysTray
sysTrayNew orientation sysTrayAlignBegin = do
  sysTrayBox <-
    View.boxUniDynNew
      (View.defBoxArg orientation)
        { View.boxPacking = if sysTrayAlignBegin then View.BoxPackStart else View.BoxPackEnd
        }
  #getStyleContext (View.boxUniDynWidget sysTrayBox) >>= flip #addClass (T.pack "system-tray-area")
  pure SysTray{..}

sysTrayCtrl :: MonadIO m => SysTray -> SysTrayOp -> m ()
sysTrayCtrl SysTray{..} = \case
  TrayAddItem item -> do
    View.boxUniDynCtrl sysTrayBox (View.BoxUniAdd $ item `asA` Gtk.Widget)
    #showAll item
  TrayRemoveItem item -> do
    #hide item
    View.boxUniDynCtrl sysTrayBox (View.BoxUniRemove $ item `asA` Gtk.Widget)
