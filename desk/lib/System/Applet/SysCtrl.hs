{-# LANGUAGE OverloadedLabels #-}

module System.Applet.SysCtrl (sysCtrlBtn) where

import Control.Concurrent.Task
import Control.Monad
import Control.Monad.IO.Class
import Data.GI.Base.Attributes
import Data.GI.Base.Signals
import Data.Text qualified as T
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.Task qualified as Gtk
import Gtk.Window qualified as Gtk
import Status.X11.XHandle
import System.FilePath
import System.Pulp.PulpEnv
import View.Imagery qualified as View
import XMonad.Util.Run (safeSpawn)

-- | System control button with shutdown symbol icon.
-- Shows the system control dialog.
sysCtrlBtn :: (MonadIO m, MonadXHand m, MonadPulpPath m) => Gtk.Window -> m Gtk.Widget
sysCtrlBtn parent = do
  watch <- runXHand sysCtrlListen
  uiFile <- pulpDataPath ("ui" </> "sysctl.ui")
  ctlWin <- liftIO $ ctrlWinNew (T.pack uiFile) parent

  icon <- View.imageStaticNew Gtk.IconSizeLargeToolbar True $ View.ImgSName (T.pack "system-shutdown-symbolic")
  wid <- Gtk.buttonNewWith (Just icon) (liftIO . void $ #showAll ctlWin)
  liftIO $ do
    killWatch <- Gtk.uiTask watch (\SysCtlMsg -> Gtk.widgetActivate wid)
    on wid #destroy killWatch
  pure wid

ctrlWinNew :: T.Text -> Gtk.Window -> IO Gtk.Window
ctrlWinNew uiFile parent = Gtk.buildFromFile uiFile $ do
  Just window <- Gtk.getElement (T.pack "sysctl") Gtk.Window

  set window [#transientFor := parent]
  #setKeepAbove window True
  Gtk.windowSetTransparent window
  Gtk.windowGrabOnMap window
  on window #keyPressEvent $
    flip get #keyval >=> \case
      Gtk.KEY_Escape -> True <$ #close window
      _ -> pure False
  on window #deleteEvent $ \_ -> #hideOnDelete window

  Gtk.addCallback (T.pack "sysctl-close") $ do #close window
  Gtk.addCallback (T.pack "sysctl-logout") $ do safeSpawn "killall" ["xmonad-manage"]
  Gtk.addCallback (T.pack "sysctl-reboot") $ do safeSpawn "systemctl" ["reboot"]
  Gtk.addCallback (T.pack "sysctl-poweroff") $ do safeSpawn "systemctl" ["poweroff"]

  pure window

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
