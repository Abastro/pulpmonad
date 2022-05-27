module Main (main) where

import Control.Monad
import Data.Text qualified as T
import Defines
import GI.Gdk.Structs.EventKey qualified as Gdk
import GI.Gtk.Objects.CssProvider qualified as Gtk
import System.Environment (getEnv)
import System.Exit
import UI.Commons qualified as UI
import UI.Containers qualified as UI
import UI.Singles qualified as UI
import UI.Window qualified as UI
import UI.Application qualified as UI
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

actOf :: UI.Window -> SysCtl -> IO ()
actOf window = \case
  Build -> do
    safeSpawn "gnome-terminal" ["--class=term-float", "--", "xmonad-manage", "build", "pulpmonad"]
    UI.windowClose window -- For now, close the window..
  Refresh -> do
    safeSpawn "xmonad" ["--restart"]
    UI.windowClose window
  Logout -> safeSpawn "killall" ["xmonad-manage"] -- A roundabout
  Reboot -> safeSpawn "systemctl" ["reboot"]
  Poweroff -> safeSpawn "systemctl" ["poweroff"]

ctlButton :: UI.Window -> SysCtl -> IO UI.Widget
ctlButton window ctl = do
  box <-
    UI.boxed UI.OrientationVertical 5
      =<< sequenceA
        [ UI.iconNewFromName UI.IconSizeDialog (iconOf ctl)
        , UI.toWidget =<< UI.labelNew (Just $ nameOf ctl)
        ]
  UI.widgetSetValign box UI.AlignCenter

  btn <- UI.buttonNewWith (Just box) $ actOf window ctl
  UI.widgetGetStyleContext btn >>= flip UI.styleContextAddClass (styleOf ctl)
  pure btn

main :: IO ()
main = do
  -- Does not care crashing here
  Just app <- UI.applicationNew (Just $ T.pack "pulp.ui.sysctl") []
  UI.onApplicationActivate app (activating app)
  status <- UI.applicationRun app Nothing
  when (status /= 0) $ exitWith (ExitFailure $ fromIntegral status)
  where
    cssProv :: IO Gtk.CssProvider
    cssProv = do
      css <- Gtk.cssProviderNew
      cfgDir <- getEnv "XMONAD_CONFIG_DIR"
      Gtk.cssProviderLoadFromPath css $ T.pack (cfgDir </> "styles" </> "pulp-sysctl.css")
      pure css

    activating :: UI.Application -> IO ()
    activating app = do
      cssProv >>= flip UI.defscreenAddStyleContext UI.STYLE_PROVIDER_PRIORITY_USER

      window <- UI.appWindowNew app
      UI.windowSetTitle window (T.pack "Pulp System Control")
      UI.windowSetDefaultSize window 560 140
      UI.windowSetTypeHint window UI.WindowTypeHintSplashscreen
      UI.windowSetPosition window UI.WindowPositionCenter
      UI.windowSetKeepAbove window True
      UI.windowSetSkipPagerHint window True
      UI.windowSetSkipTaskbarHint window True
      UI.onWidgetKeyPressEvent window $
        Gdk.getEventKeyKeyval >=> \case
          UI.KEY_Escape -> True <$ UI.windowClose window
          _ -> pure False

      UI.windowAsTransparent window
      UI.windowGrabOnMap window

      UI.containerAdd window =<< btns window
      UI.widgetShowAll window

    btns :: UI.Window -> IO UI.Widget
    btns window = do
      box <-
        UI.homogBoxed UI.OrientationHorizontal 10
          =<< traverse (ctlButton window) [minBound .. maxBound]
      UI.widgetGetStyleContext box >>= flip UI.styleContextAddClass (T.pack "btn-area")
      UI.widgetSetHalign box UI.AlignFill
      pure box
