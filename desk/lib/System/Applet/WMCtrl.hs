{-# LANGUAGE OverloadedLabels #-}

module System.Applet.WMCtrl (wmCtrlBtn) where

import Control.Concurrent
import Control.Concurrent.Task
import Control.Exception
import Control.Monad.IO.Class
import Data.Foldable
import Data.Function hiding (on)
import Data.GI.Base.Attributes
import Data.GI.Base.Signals
import Data.Text qualified as T
import Data.Text.IO qualified as T
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
      -- TODO: need fix "waitForProcess, child process does not exist".
      forkIO . bracket_ begin end . handle @IOException onError $ do
        let prog = proc "xmonad-manage" ["build", "pulpmonad"]
        -- Creates pipe for merging streams
        bracket createPipe (\(r, w) -> hClose r >> hClose w) $ \(reads, writes) -> do
          withCreateProcess prog{std_out = UseHandle writes, std_err = UseHandle writes} $
            \_ _ _ _ -> do
              actOnLine reads $ \txt -> Gtk.uiSingleRun (addLine txt)
      pure ()
      where
        begin = Gtk.uiSingleRun (setMode True)
        end = threadDelay 3000000 >> Gtk.uiSingleRun (setMode False)

        actOnLine outp act = fix $ \recurse ->
          hIsEOF outp >>= \case
            True -> pure ()
            False -> (T.hGetLine outp >>= act) >> recurse

        onError err = do
          Gtk.uiSingleRun (addLine . T.pack $ show err)
          throwIO err

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
ctrlViewNew uiFile onAct parent = Gtk.buildFromFile uiFile $ do
  Just window <- Gtk.getElement (T.pack "wmctl") Gtk.Window
  Just stack <- Gtk.getElement (T.pack "wmctl-stack") Gtk.Stack
  Just buildScr <- Gtk.getElement (T.pack "wmctl-build-scroll") Gtk.ScrolledWindow
  Just buildLab <- Gtk.getElement (T.pack "wmctl-build-label") Gtk.Label

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
  for_ [minBound .. maxBound] $ \ctrl -> Gtk.addCallback (ctrlSignal ctrl) (onAct view ctrl)

  pure view
