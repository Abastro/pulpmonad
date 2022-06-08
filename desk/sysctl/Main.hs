module Main (main) where

import Control.Monad
import Data.Text qualified as T
import Defines
import GI.Gdk.Structs.EventKey qualified as Gdk
import Gtk.Application qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.Styles qualified as Gtk
import Gtk.Window qualified as Gtk
import System.Environment (getEnv)
import System.Exit
import View.Boxes qualified as View
import View.Imagery qualified as View
import View.Textual qualified as View
import XMonad.Util.Run (safeSpawn)

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
    Gtk.windowClose window -- For now, close the window..
  Refresh -> do
    safeSpawn "xmonad" ["--restart"]
    Gtk.windowClose window
  Logout -> safeSpawn "killall" ["xmonad-manage"] -- A roundabout
  Reboot -> safeSpawn "systemctl" ["reboot"]
  Poweroff -> safeSpawn "systemctl" ["poweroff"]

ctlButton :: Gtk.Window -> SysCtl -> IO Gtk.Widget
ctlButton window ctl = do
  box <-
    View.boxStaticNew (View.defBoxArg Gtk.OrientationVertical){View.boxSpacing = 5}
      =<< sequenceA
        [ View.imageStaticNew Gtk.IconSizeDialog (View.ImgSName $ iconOf ctl)
        , View.labelStaticNew View.defLabelArg (nameOf ctl)
        ]
  Gtk.widgetSetValign box Gtk.AlignCenter

  btn <- Gtk.buttonNewWith (Just box) $ actOf window ctl
  Gtk.widgetGetStyleContext btn >>= flip Gtk.styleContextAddClass (styleOf ctl)
  pure btn

main :: IO ()
main = do
  -- Does not care crashing here
  Just app <- Gtk.applicationNew (Just $ T.pack "pulp.Gtk.sysctl") []
  Gtk.onApplicationActivate app (activating app)
  status <- Gtk.applicationRun app Nothing
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
      cssProv >>= flip Gtk.defScreenAddStyleContext Gtk.STYLE_PROVIDER_PRIORITY_USER

      window <- Gtk.appWindowNew app
      Gtk.windowSetTitle window (T.pack "Pulp System Control")
      Gtk.windowSetDefaultSize window 560 140
      Gtk.windowSetTypeHint window Gtk.WindowTypeHintSplashscreen
      Gtk.windowSetPosition window Gtk.WindowPositionCenter
      Gtk.windowSetKeepAbove window True
      Gtk.windowSetSkipPagerHint window True
      Gtk.windowSetSkipTaskbarHint window True
      Gtk.onWidgetKeyPressEvent window $
        Gdk.getEventKeyKeyval >=> \case
          Gtk.KEY_Escape -> True <$ Gtk.windowClose window
          _ -> pure False

      Gtk.windowSetTransparent window
      Gtk.windowGrabOnMap window

      Gtk.containerAdd window =<< btns window
      Gtk.widgetShowAll window

    btns :: Gtk.Window -> IO Gtk.Widget
    btns window = do
      box <-
        View.boxStaticNew (View.defBoxArg Gtk.OrientationHorizontal){View.boxSpacing = 10, View.boxHomogeneous = True}
          =<< traverse (ctlButton window) [minBound .. maxBound]
      Gtk.widgetSetName box (T.pack "pulp-sysctl")
      Gtk.widgetGetStyleContext box >>= flip Gtk.styleContextAddClass (T.pack "btn-area")
      Gtk.widgetSetHalign box Gtk.AlignFill
      pure box
