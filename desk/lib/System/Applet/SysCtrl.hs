{-# LANGUAGE OverloadedLabels #-}

module System.Applet.SysCtrl (sysCtrlBtn) where

import Control.Monad
import Control.Monad.IO.Class
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.Text qualified as T
import GI.Gtk.Objects.Label qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.Window qualified as Gtk
import View.Boxes qualified as View
import View.Imagery qualified as View
import XMonad.Util.Run (safeSpawn)

-- | System control button with shutdown symbol icon.
-- Shows the system control dialog.
sysCtrlBtn :: MonadIO m => m Gtk.Widget
sysCtrlBtn = do
  icon <- View.imageStaticNew Gtk.IconSizeLargeToolbar True $ View.ImgSName (T.pack "system-shutdown-symbolic")
  Gtk.buttonNewWith (Just icon) (liftIO . void $ sysCtrlWin)

data SysCtl = Build | Refresh | Logout | Reboot | Poweroff
  deriving (Enum, Bounded, Show)

nameOf :: SysCtl -> T.Text
nameOf = \case
  Build -> T.pack "Build"
  Refresh -> T.pack "Refresh"
  Logout -> T.pack "Log out"
  Reboot -> T.pack "Reboot"
  Poweroff -> T.pack "Power off"

iconOf :: SysCtl -> T.Text
iconOf = \case
  Build -> T.pack "document-save-symbolic"
  Refresh -> T.pack "view-refresh-symbolic"
  Logout -> T.pack "system-log-out-symbolic"
  Reboot -> T.pack "system-reboot-symbolic"
  Poweroff -> T.pack "system-shutdown-symbolic"

styleOf :: SysCtl -> T.Text
styleOf = \case
  Build -> T.pack "btn-sys"
  Refresh -> T.pack "btn-sys"
  Logout -> T.pack "btn-logout"
  Reboot -> T.pack "btn-reboot"
  Poweroff -> T.pack "btn-poweroff"

actOf :: Gtk.Window -> SysCtl -> IO ()
actOf window = \case
  Build -> do
    safeSpawn "gnome-terminal" ["--class=term-float", "--", "xmonad-manage", "build", "pulpmonad"]
    #close window -- For now, close the window..
  Refresh -> do
    safeSpawn "xmonad" ["--restart"]
    #close window
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

-- TODO A way to expose this window from WM message
sysCtrlWin :: IO Gtk.Window
sysCtrlWin = do
  window <-
    new
      Gtk.Window
      [ #title := T.pack "Pulp System Control"
      , #typeHint := Gtk.WindowTypeHintSplashscreen
      , #windowPosition := Gtk.WindowPositionCenter
      , #skipPagerHint := True
      , #skipTaskbarHint := True
      , #modal := True
      ]
  #setDefaultSize window 560 140
  #setKeepAbove window True

  Gtk.onWidgetKeyPressEvent window $
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
