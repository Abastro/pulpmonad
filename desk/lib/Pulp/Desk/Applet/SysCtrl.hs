{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}

module Pulp.Desk.Applet.SysCtrl (sysCtrlBtn) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.GI.Base.Attributes qualified as GI
import Data.GI.Base.Signals qualified as GI
import Data.Text qualified as T
import GI.Gdk.Unions.Event qualified as Gdk
import GI.Gtk.Objects.Widget qualified as Gtk
import Graphics.X11.Types qualified as X11
import Graphics.X11.Xlib.Extras qualified as X11
import Pulp.Desk.Env.PulpEnv
import Pulp.Desk.PulpPath
import Pulp.Desk.Reactive.Entry
import Pulp.Desk.System.X11.XHandle qualified as X11
import Pulp.Desk.UI.Commons qualified as Gtk
import Pulp.Desk.UI.Reactive qualified as Gtk
import Pulp.Desk.UI.Window qualified as Gtk
import Reactive.Banana.Frameworks
import XMonad.Util.Run (safeSpawn)

-- | System control button with shutdown symbol icon.
-- Shows the system control dialog.
sysCtrlBtn :: Gtk.Window -> PulpIO Gtk.Widget
sysCtrlBtn parent = withRunInIO $ \unlift -> do
  watch <- unlift $ X11.runXHook sysCtrlListen
  uiFile <- dataPath ("ui" </> "sysctl.ui")
  View{..} <- view (T.pack uiFile) parent

  network <- compile $ do
    callEvent <- sourceEvent watch
    actEvent <- sourceEvent toAct

    -- Problem: Calling "openWindow" somehow does not grab focus correctly.
    reactimate (Gtk.uiSingleRun ctrlButton.activate <$ callEvent)
    reactimate (actOn <$> actEvent)
  actuate network

  pure ctrlButton
  where
    actOn Logout = safeSpawn "killall" ["xmonad-manage"]
    actOn Reboot = safeSpawn "systemctl" ["reboot"]
    actOn Poweroff = safeSpawn "systemctl" ["poweroff"]

{-------------------------------------------------------------------
                              View
--------------------------------------------------------------------}

data Action = Logout | Reboot | Poweroff

data View = View
  { ctrlButton :: !Gtk.Widget
  , openWindow :: IO ()
  , toAct :: Source Action
  }

view :: T.Text -> Gtk.Window -> IO View
view uiFile parent = Gtk.buildFromFile uiFile $ do
  Just ctrlButton <- Gtk.getElement (T.pack "btn-sysctl") Gtk.Widget
  Just window <- Gtk.getElement (T.pack "sysctl") Gtk.Window

  -- Construct window at top
  GI.set window [#transientFor GI.:= parent]
  window.setKeepAbove True
  Gtk.windowSetTransparent window
  Gtk.windowGrabOnMap window
  GI.on window #deleteEvent $ \_ -> window.hideOnDelete

  let openWindow = window.showAll

  (toAct, act) <- liftIO sourceSink
  Gtk.addCallback (T.pack "sysctl-open") openWindow
  Gtk.addCallbackWithEvent (T.pack "sysctl-keypress") Gdk.getEventKey $ onKeyPress window
  Gtk.addCallback (T.pack "sysctl-close") $ window.close
  Gtk.addCallback (T.pack "sysctl-logout") $ act Logout
  Gtk.addCallback (T.pack "sysctl-reboot") $ act Reboot
  Gtk.addCallback (T.pack "sysctl-poweroff") $ act Poweroff

  pure View{..}
  where
    -- Apparently this requires GADTs extension
    onKeyPress window event =
      GI.get event #keyval >>= \case
        Gtk.KEY_Escape -> #close window
        _ -> pure ()

{-------------------------------------------------------------------
                          Communication
--------------------------------------------------------------------}

data SysCtrlCall = SysCtrlCall

sysCtrlListen :: X11.XIO (Source SysCtrlCall)
sysCtrlListen = do
  rootWin <- X11.xWindow
  ctrlTyp <- X11.xAtom "_XMONAD_CTRL_MSG"
  ctrlSys <- X11.xAtom "_XMONAD_CTRL_SYS"
  X11.xListenSource X11.structureNotifyMask rootWin $ \case
    X11.ClientMessageEvent{ev_message_type = msgTyp, ev_data = subTyp : _}
      | msgTyp == ctrlTyp
      , fromIntegral subTyp == ctrlSys -> do
          pure (Just SysCtrlCall)
    _ -> pure Nothing
