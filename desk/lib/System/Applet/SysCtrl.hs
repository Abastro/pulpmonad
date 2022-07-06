{-# LANGUAGE OverloadedLabels #-}

module System.Applet.SysCtrl (sysCtrlBtn) where

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

-- | System control button with shutdown symbol icon.
-- Shows the system control dialog.
sysCtrlBtn :: (MonadIO m, MonadXHand m) => Gtk.Window -> m Gtk.Widget
sysCtrlBtn parent = do
  watch <- runXHand sysCtrlListen
  icon <- View.imageStaticNew Gtk.IconSizeLargeToolbar True $ View.ImgSName (T.pack "system-shutdown-symbolic")
  wid <- Gtk.buttonNewWith (Just icon) (liftIO . void $ sysCtrlWin parent)
  liftIO $ do
    killWatch <- Gtk.uiTask watch (\SysCtlMsg -> Gtk.widgetActivate wid)
    on wid #destroy killWatch
  pure wid

data SysCtl = Cancel | Logout | Reboot | Poweroff
  deriving (Enum, Bounded, Show)

nameOf :: SysCtl -> T.Text
nameOf = \case
  Cancel -> T.pack "Cancel"
  Logout -> T.pack "Log out"
  Reboot -> T.pack "Reboot"
  Poweroff -> T.pack "Power off"

iconOf :: SysCtl -> T.Text
iconOf = \case
  Cancel -> T.pack "window-close-symbolic"
  Logout -> T.pack "system-log-out-symbolic"
  Reboot -> T.pack "system-reboot-symbolic"
  Poweroff -> T.pack "system-shutdown-symbolic"

styleOf :: SysCtl -> T.Text
styleOf = \case
  Cancel -> T.pack "btn-sys"
  Logout -> T.pack "btn-logout"
  Reboot -> T.pack "btn-reboot"
  Poweroff -> T.pack "btn-poweroff"

actOf :: Gtk.Window -> SysCtl -> IO ()
actOf window = \case
  Cancel -> #close window
  Logout -> safeSpawn "killall" ["xmonad-manage"] -- A roundabout
  Reboot -> safeSpawn "systemctl" ["reboot"]
  Poweroff -> safeSpawn "systemctl" ["poweroff"]

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

sysCtrlWin :: Gtk.Window -> IO Gtk.Window
sysCtrlWin parent = do
  window <-
    new
      Gtk.Window
      [ #title := T.pack "Pulp System Control"
      , #type := Gtk.WindowTypePopup
      , #windowPosition := Gtk.WindowPositionCenter
      , #skipPagerHint := True
      , #skipTaskbarHint := True
      , #transientFor := parent
      , #modal := True
      ]
  #setDefaultSize window 460 140
  #setKeepAbove window True

  on window #keyPressEvent $
    flip get #keyval >=> \case
      Gtk.KEY_Escape -> True <$ #close window
      _ -> pure False

  Gtk.windowSetTransparent window
  Gtk.windowGrabOnMap window

  #add window =<< btns window
  #showAll window

  pure window
  where
    btns :: Gtk.Window -> IO Gtk.Widget
    btns window = do
      box <-
        View.boxStaticNew (View.defBoxArg Gtk.OrientationHorizontal){View.boxSpacing = 10, View.boxHomogeneous = True}
          =<< traverse (ctlButton window) [minBound .. maxBound]
      #setName box (T.pack "pulp-sysctl")
      #getStyleContext box >>= flip #addClass (T.pack "btn-area")
      #setHalign box Gtk.AlignFill
      pure box

{-------------------------------------------------------------------
                          Communication
--------------------------------------------------------------------}

data SysCtlMsg = SysCtlMsg

sysCtrlListen :: XIO () (Task SysCtlMsg)
sysCtrlListen = do
  rootWin <- xWindow
  ctrlTyp <- xAtom "_XMONAD_CTRL_MSG"
  ctrlSys <- xAtom "_XMONAD_CTRL_SYS"
  xListenTo structureNotifyMask rootWin Nothing $ \case
    ClientMessageEvent{ev_message_type = msgTyp, ev_data = subTyp : _}
      | msgTyp == ctrlTyp
        , fromIntegral subTyp == ctrlSys -> do
        pure (Just SysCtlMsg)
    _ -> pure Nothing
