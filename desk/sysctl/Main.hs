{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Control.Monad
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.Text qualified as T
import Defines
import GI.Gtk.Objects.Label qualified as Gtk
import Gtk.Application qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.Styles qualified as Gtk
import Gtk.Window qualified as Gtk
import System.Environment (getEnv)
import System.Exit
import View.Boxes qualified as View
import View.Imagery qualified as View
import XMonad.Util.Run (safeSpawn)

-- TODO Internalize to the pulp-taskbar

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

main :: IO ()
main = do
  -- Does not care crashing here
  Just app <- Gtk.applicationNew (Just $ T.pack "pulp.ui.sysctl") []
  Gtk.onApplicationActivate app (activating app)
  status <- Gtk.applicationRun app Nothing
  when (status /= 0) $ exitWith (ExitFailure $ fromIntegral status)
  where
    cssProv :: IO Gtk.CssProvider
    cssProv = do
      css <- new Gtk.CssProvider []
      cfgDir <- getEnv "XMONAD_DATA_DIR"
      #loadFromPath css $ T.pack (cfgDir </> "styles" </> "pulp-sysctl.css")
      pure css

    activating :: Gtk.Application -> IO ()
    activating app = do
      cssProv >>= flip Gtk.defScreenAddStyleContext Gtk.STYLE_PROVIDER_PRIORITY_USER

      window <- Gtk.appWindowNew app
      #setDefaultSize window 560 140
      set
        window
        [ #title := T.pack "Pulp System Control"
        , #typeHint := Gtk.WindowTypeHintSplashscreen
        , #windowPosition := Gtk.WindowPositionCenter
        , #skipPagerHint := True
        , #skipTaskbarHint := True
        ]
      #setKeepAbove window True

      Gtk.onWidgetKeyPressEvent window $
        flip get #keyval >=> \case
          Gtk.KEY_Escape -> True <$ #close window
          _ -> pure False

      Gtk.windowSetTransparent window
      Gtk.windowGrabOnMap window

      #add window =<< btns window
      #showAll window

    btns :: Gtk.Window -> IO Gtk.Widget
    btns window = do
      box <-
        View.boxStaticNew (View.defBoxArg Gtk.OrientationHorizontal){View.boxSpacing = 10, View.boxHomogeneous = True}
          =<< traverse (ctlButton window) [minBound .. maxBound]
      #setName box (T.pack "pulp-sysctl")
      #getStyleContext box >>= flip #addClass (T.pack "btn-area")
      #setHalign box Gtk.AlignFill
      pure box
