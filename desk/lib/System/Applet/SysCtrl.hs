{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}

module System.Applet.SysCtrl (sysCtrlBtn) where

import Control.Concurrent.Task
import Control.Event.Entry
import Control.Monad.IO.Class
import Data.GI.Base.Attributes
import Data.GI.Base.Signals
import Data.Text qualified as T
import GI.Gdk.Unions.Event qualified as Gdk
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
import Gtk.Commons qualified as Gtk
import Gtk.Task qualified as Gtk
import Gtk.Window qualified as Gtk
import Reactive.Banana.Frameworks
import Status.X11.XHandle
import System.FilePath
import XMonad.Util.Run (safeSpawn)
import System.Pulp.PulpPath
import Control.Monad.IO.Unlift

-- | System control button with shutdown symbol icon.
-- Shows the system control dialog.
sysCtrlBtn :: (MonadUnliftIO m, MonadXHand m) => Gtk.Window -> m Gtk.Widget
sysCtrlBtn parent = withRunInIO $ \unlift -> do
  watch <- unlift $ runXHand sysCtrlListen
  uiFile <- dataPath ("ui" </> "sysctl.ui")
  View{..} <- view (T.pack uiFile) parent

  network <- compile $ do
    callEvent <- sourceEvent (taskToSource watch)
    actEvent <- sourceEvent toAct

    -- Problem: Calling "openWindow" somehow does not grab focus correctly.
    reactimate (Gtk.uiSingleRun (Gtk.widgetActivate ctrlButton) <$ callEvent)
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
  set window [#transientFor := parent]
  #setKeepAbove window True
  Gtk.windowSetTransparent window
  Gtk.windowGrabOnMap window
  on window #deleteEvent $ \_ -> #hideOnDelete window

  let openWindow = #showAll window

  (toAct, act) <- liftIO sourceSink
  Gtk.addCallback (T.pack "sysctl-open") openWindow
  Gtk.addCallbackWithEvent (T.pack "sysctl-keypress") Gdk.getEventKey $ onKeyPress window
  Gtk.addCallback (T.pack "sysctl-close") $ #close window
  Gtk.addCallback (T.pack "sysctl-logout") $ act Logout
  Gtk.addCallback (T.pack "sysctl-reboot") $ act Reboot
  Gtk.addCallback (T.pack "sysctl-poweroff") $ act Poweroff

  pure View{..}
  where
    -- Apparently this requires GADTs extension
    onKeyPress window event =
      get event #keyval >>= \case
        Gtk.KEY_Escape -> #close window
        _ -> pure ()

{-------------------------------------------------------------------
                          Communication
--------------------------------------------------------------------}

data SysCtrlCall = SysCtrlCall

sysCtrlListen :: XIO (Task SysCtrlCall)
sysCtrlListen = do
  rootWin <- xWindow
  ctrlTyp <- xAtom "_XMONAD_CTRL_MSG"
  ctrlSys <- xAtom "_XMONAD_CTRL_SYS"
  xListenTo structureNotifyMask rootWin Nothing $ \case
    ClientMessageEvent{ev_message_type = msgTyp, ev_data = subTyp : _}
      | msgTyp == ctrlTyp
      , fromIntegral subTyp == ctrlSys -> do
          pure (Just SysCtrlCall)
    _ -> pure Nothing
