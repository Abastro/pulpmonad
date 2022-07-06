{-# LANGUAGE OverloadedLabels #-}

module System.Applet.WMCtrl (wmCtrlBtn) where

import Control.Concurrent.Task
import Control.Monad
import Control.Monad.IO.Class
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.GI.Base.Signals
import Data.Text qualified as T
import GI.Gtk.Objects.Label qualified as Gtk
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.Task qualified as Gtk
import Gtk.Window qualified as Gtk
import Status.X11.XHandle
import View.Boxes qualified as View
import View.Imagery qualified as View
import XMonad.Util.Run (safeSpawn)

-- | Window manager control button.
-- Shows the system control dialog.
wmCtrlBtn :: (MonadIO m, MonadXHand m) => Gtk.Window -> m Gtk.Widget
wmCtrlBtn parent = do
  watch <- runXHand wmCtrlListen
  icon <- View.imageStaticNew Gtk.IconSizeLargeToolbar True $ View.ImgSName (T.pack "system-settings-symbolic")
  wid <- Gtk.buttonNewWith (Just icon) (liftIO . void $ wmCtrlWin parent)
  liftIO $ do
    killWatch <- Gtk.uiTask watch (\WMCtlMsg -> Gtk.widgetActivate wid)
    on wid #destroy killWatch
  pure wid

data SysCtl = Cancel | Build | Refresh
  deriving (Enum, Bounded, Show)

nameOf :: SysCtl -> T.Text
nameOf = \case
  Cancel -> T.pack "Cancel"
  Build -> T.pack "Build"
  Refresh -> T.pack "Refresh"

iconOf :: SysCtl -> T.Text
iconOf = \case
  Cancel -> T.pack "window-close-symbolic"
  Build -> T.pack "document-save-symbolic"
  Refresh -> T.pack "view-refresh-symbolic"

styleOf :: SysCtl -> T.Text
styleOf = \case
  Cancel -> T.pack "btn-ctl"
  Build -> T.pack "btn-ctl"
  Refresh -> T.pack "btn-ctl"

actOf :: Gtk.Window -> SysCtl -> IO ()
actOf window = \case
  Cancel -> #close window
  Build -> do
    safeSpawn "gnome-terminal" ["--class=term-float", "--", "xmonad-manage", "build", "pulpmonad"]
    #close window -- For now, close the window..
  Refresh -> do
    safeSpawn "xmonad" ["--restart"]
    #close window

ctlButton :: Gtk.Window -> SysCtl -> IO Gtk.Widget
ctlButton window ctl = do
  box <-
    View.boxStaticNew (View.defBoxArg Gtk.OrientationVertical){View.boxSpacing = 5}
      =<< sequenceA
        [ View.imageStaticNew Gtk.IconSizeDialog True (View.ImgSName $ iconOf ctl)
        , Gtk.toWidget =<< new Gtk.Label [#label := nameOf ctl]
        ]
  #setValign box Gtk.AlignCenter

  btn <- Gtk.buttonNewWith (Just box) $ actOf window ctl
  #getStyleContext btn >>= flip #addClass (styleOf ctl)
  pure btn

wmCtrlWin :: Gtk.Window -> IO Gtk.Window
wmCtrlWin parent = do
  window <-
    new
      Gtk.Window
      [ #title := T.pack "Pulp Manager Control"
      , #typeHint := Gtk.WindowTypeHintDialog
      , #windowPosition := Gtk.WindowPositionCenter
      , #transientFor := parent
      , #modal := True
      ]
  #setDefaultSize window 360 140
  #setKeepAbove window True
  #setIconName window (Just $ T.pack "system-settings")

  on window #keyPressEvent $
    flip get #keyval >=> \case
      Gtk.KEY_Escape -> True <$ #close window
      _ -> pure False

  Gtk.windowSetTransparent window

  #add window =<< btns window
  #showAll window

  pure window
  where
    btns :: Gtk.Window -> IO Gtk.Widget
    btns window = do
      box <-
        View.boxStaticNew (View.defBoxArg Gtk.OrientationHorizontal){View.boxSpacing = 10, View.boxHomogeneous = True}
          =<< traverse (ctlButton window) [minBound .. maxBound]
      #setName box (T.pack "pulp-wmctl")
      #getStyleContext box >>= flip #addClass (T.pack "btn-area")
      #setHalign box Gtk.AlignFill
      pure box

{-------------------------------------------------------------------
                          Communication
--------------------------------------------------------------------}

data WMCtlMsg = WMCtlMsg

wmCtrlListen :: XIO () (Task WMCtlMsg)
wmCtrlListen = do
  rootWin <- xWindow
  ctrlTyp <- xAtom "_XMONAD_CTRL_MSG"
  ctrlSys <- xAtom "_XMONAD_CTRL_WM"
  xListenTo structureNotifyMask rootWin Nothing $ \case
    ClientMessageEvent{ev_message_type = msgTyp, ev_data = subTyp : _}
      | msgTyp == ctrlTyp
        , fromIntegral subTyp == ctrlSys -> do
        pure (Just WMCtlMsg)
    _ -> pure Nothing
