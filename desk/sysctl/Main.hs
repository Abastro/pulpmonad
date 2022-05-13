module Main (main) where

import Control.Monad
import Data.Foldable
import Data.Text qualified as T
import Defines
import GI.Gdk.Constants qualified as Gdk
import GI.Gdk.Enums qualified as Gdk
import GI.Gdk.Objects.Screen qualified as Gdk
import GI.Gdk.Structs.EventKey qualified as Gdk
import GI.Gio.Objects.Application qualified as Gio
import GI.Gtk.Constants qualified as Gtk
import GI.Gtk.Objects.Application qualified as Gtk
import GI.Gtk.Objects.ApplicationWindow qualified as Gtk
import GI.Gtk.Objects.Box qualified as Gtk
import GI.Gtk.Objects.CssProvider qualified as Gtk
import GI.Gtk.Objects.Label qualified as Gtk
import GI.Gtk.Objects.Window qualified as Gtk
import GtkCommons qualified as Gtk
import System.Environment (getEnv)
import System.Exit
import XMonad.Util.Run (safeSpawn)

data SysCtl = Build | Refresh | Logout | Reboot | Poweroff
  deriving (Enum, Bounded, Show)

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
    Gtk.windowClose window -- For now, close the window..
  Refresh -> do
    safeSpawn "xmonad" ["--restart"]
    Gtk.windowClose window
  Logout -> safeSpawn "killall" ["xmonad-manage"] -- A roundabout
  Reboot -> safeSpawn "systemctl" ["reboot"]
  Poweroff -> safeSpawn "systemctl" ["poweroff"]

ctlButton :: Gtk.Window -> SysCtl -> IO Gtk.Widget
ctlButton window ctl = do
  box <- do
    box <- Gtk.boxNew Gtk.OrientationVertical 5
    Gtk.widgetSetValign box Gtk.AlignCenter
    Gtk.containerAdd box =<< Gtk.iconNewFromName Gtk.IconSizeDialog (iconOf ctl)
    Gtk.containerAdd box =<< Gtk.labelNew (Just . T.pack $ show ctl)
    Gtk.toWidget box

  btn <- Gtk.buttonNewWith box $ actOf window ctl
  Gtk.widgetGetStyleContext btn >>= flip Gtk.styleContextAddClass (styleOf ctl)
  pure btn

main :: IO ()
main = do
  -- Does not care crashing here
  Just app <- Gtk.applicationNew (Just $ T.pack "pulp.ui.sysctl") []
  Gio.onApplicationActivate app (activating app)
  status <- Gio.applicationRun app Nothing
  when (status /= 0) $ exitWith (ExitFailure $ fromIntegral status)
  where
    cssProv :: IO Gtk.CssProvider
    cssProv = do
      css <- Gtk.cssProviderNew
      cfgDir <- getEnv "XMONAD_CONFIG_DIR"
      Gtk.cssProviderLoadFromPath css $ T.pack (cfgDir </> "styles" </> "pulp-sysctl.css")
      pure css

    activating :: Gtk.Application -> IO ()
    activating app = do
      Just screen <- Gdk.screenGetDefault
      css <- cssProv
      Gtk.styleContextAddProviderForScreen screen css $ fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER

      window <- Gtk.applicationWindowNew app >>= Gtk.toWindow
      Gtk.windowSetTitle window (T.pack "Pulp System Control")
      Gtk.windowSetDefaultSize window 560 140
      Gtk.windowSetTypeHint window Gdk.WindowTypeHintSplashscreen
      Gtk.windowSetPosition window Gtk.WindowPositionCenterAlways
      Gtk.windowSetKeepAbove window True
      Gtk.windowSetSkipPagerHint window True
      Gtk.windowSetSkipTaskbarHint window True
      Gtk.onWidgetKeyPressEvent window $
        Gdk.getEventKeyKeyval >=> \case
          Gdk.KEY_Escape -> True <$ Gtk.windowClose window
          _ -> pure False

      Gtk.windowAsTransparent window
      Gtk.windowGrabOnMap window

      btns window >>= Gtk.containerAdd window
      Gtk.widgetShowAll window

    btns :: Gtk.Window -> IO Gtk.Box
    btns window = do
      box <- Gtk.boxNew Gtk.OrientationHorizontal 10
      Gtk.widgetGetStyleContext box >>= flip Gtk.styleContextAddClass (T.pack "btn-area")
      Gtk.widgetSetHalign box Gtk.AlignFill
      Gtk.boxSetHomogeneous box True

      buttons <- traverse (ctlButton window) [minBound .. maxBound]
      traverse_ (Gtk.containerAdd box) buttons
      pure box
