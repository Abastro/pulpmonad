{-# LANGUAGE OverloadedLabels #-}

module System.Applet.WMCtrl (wmCtrlBtn) where

import Control.Concurrent
import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.Foldable
import Data.GI.Base.Attributes
import Data.GI.Base.Signals
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Foreign.Ptr (nullPtr)
import GI.Gtk.Objects.Builder qualified as Gtk
import GI.Gtk.Objects.Label qualified as Gtk
import GI.Gtk.Objects.ScrolledWindow qualified as Gtk
import GI.Gtk.Objects.Stack qualified as Gtk
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.Task qualified as Gtk
import Gtk.Window qualified as Gtk
import Status.X11.XHandle
import System.FilePath
import System.IO
import System.Process
import System.Pulp.PulpEnv
import View.Imagery qualified as View
import XMonad.Util.Run (safeSpawn)

-- | Window manager control button.
-- Shows the system control dialog.
wmCtrlBtn :: (MonadIO m, MonadXHand m, MonadPulpPath m) => Gtk.Window -> m Gtk.Widget
wmCtrlBtn parent = do
  watch <- runXHand wmCtrlListen
  ctrlWin <- ctrlWinNew parent

  icon <- View.imageStaticNew Gtk.IconSizeLargeToolbar True $ View.ImgSName (T.pack "system-settings-symbolic")
  wid <- Gtk.buttonNewWith (Just icon) (#showAll ctrlWin)
  liftIO $ do
    killWatch <- Gtk.uiTask watch (\WMCtlMsg -> Gtk.widgetActivate wid)
    on wid #destroy killWatch
  pure wid

ctrlWinNew :: (MonadIO m, MonadPulpPath m) => Gtk.Window -> m Gtk.Window
ctrlWinNew parent = do
  uiFile <- pulpDataPath ("ui" </> "wmctl.ui")
  CtrlWinView{..} <- liftIO $ ctrlViewNew (T.pack uiFile) onAct parent
  liftIO $ setBuildmode False
  pure ctrlWin
  where
    onAct CtrlWinView{..} = \case
      Close -> #close ctrlWin
      Build -> runBuild setBuildmode addBuildLine
      Refresh -> do
        safeSpawn "xmonad" ["--restart"]
        #close ctrlWin

    runBuild setMode addLine = do
      Gtk.uiSingleRun (setMode True)
      -- FIXME: this causes error in waitForProcess, child process does not exist.
      forkIO . withCreateProcess (proc "xmonad-manage" ["build", "pulpmonad"]){std_out = CreatePipe} $
        \_ (Just outp) _ _ -> do
          actOnLine outp $ \txt -> Gtk.uiSingleRun (addLine txt)
          Gtk.uiSingleRun (setMode False)
      pure ()
      where
        actOnLine outp act =
          hIsEOF outp >>= \case
            True -> pure ()
            False -> do
              T.hGetLine outp >>= act
              actOnLine outp act

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

{-------------------------------------------------------------------
                              View
--------------------------------------------------------------------}

data WinCtrl = Close | Build | Refresh
  deriving (Eq, Ord, Enum, Bounded, Show)

ctrlSignal :: WinCtrl -> T.Text
ctrlSignal = \case
  Close -> T.pack "wmctl-close"
  Build -> T.pack "wmctl-build"
  Refresh -> T.pack "wmctl-refresh"

data CtrlWinView = CtrlWinView
  { ctrlWin :: !Gtk.Window
  , setBuildmode :: Bool -> IO ()
  , addBuildLine :: T.Text -> IO ()
  }

ctrlViewNew :: T.Text -> (CtrlWinView -> WinCtrl -> IO ()) -> Gtk.Window -> IO CtrlWinView
ctrlViewNew uiFile onAct parent = do
  builder <- Gtk.builderNewFromFile uiFile

  Just window <- Gtk.elementAs builder (T.pack "wmctl") Gtk.Window
  Just stack <- Gtk.elementAs builder (T.pack "wmctl-stack") Gtk.Stack
  Just buildScr <- Gtk.elementAs builder (T.pack "wmctl-build-scroll") Gtk.ScrolledWindow
  Just buildLab <- Gtk.elementAs builder (T.pack "wmctl-build-label") Gtk.Label

  set window [#transientFor := parent]
  Gtk.windowSetTransparent window
  on window #deleteEvent $ \_ -> #hideOnDelete window

  let setBuildmode = \case
        False -> do
          set buildLab [#label := T.empty]
          set stack [#visibleChildName := T.pack "main"]
        True -> set stack [#visibleChildName := T.pack "build"]
      addBuildLine line = do
        set buildLab [#label :~ (<> line <> T.pack "\n")]
        adj <- get buildScr #vadjustment
        set adj [#value :=> get adj #upper]

  let view = CtrlWinView{ctrlWin = window, ..}

  for_ [minBound .. maxBound] $ \ctrl ->
    #addCallbackSymbol builder (ctrlSignal ctrl) (onAct view ctrl)
  #connectSignals builder nullPtr

  pure view
