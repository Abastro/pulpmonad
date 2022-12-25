{-# LANGUAGE OverloadedLabels #-}

module System.Applet.WMCtrl (wmCtrlBtn) where

import Control.Concurrent
import Control.Concurrent.Task
import Control.Event.Entry
import Control.Exception
import Control.Monad.IO.Class
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
import Gtk.Task qualified as Gtk
import Gtk.Window qualified as Gtk
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Status.X11.XHandle
import System.FilePath
import System.IO
import System.Process
import System.Pulp.PulpEnv
import XMonad.Util.Run (safeSpawn)

-- | Window manager control button.
-- Shows the system control dialog.
wmCtrlBtn :: (MonadIO m, MonadXHand m, MonadPulpPath m) => Gtk.Window -> m Gtk.Widget
wmCtrlBtn parent = do
  watch <- runXHand wmCtrlListen
  uiFile <- pulpDataPath ("ui" </> "wmctl.ui")
  View{..} <- liftIO $ view (T.pack uiFile) parent

  network <- liftIO . compile $ do
    buildEvent <- srcEvent toBuild
    refreshEvent <- srcEvent toRefresh

    reactimate (onRefresh <$ refreshEvent)
    builds <- execute (onBuild <$ buildEvent)
    tab <- switchB (pure Main) $ fst <$> builds
    buildTxt <- switchB mempty $ snd <$> builds
    -- TODO Type-annotate the need of synchronization for GTK actions
    syncBehavior tab $ Gtk.uiSingleRun . setTab
    syncBehavior buildTxt $ Gtk.uiSingleRun . setBuildText
    pure ()

  liftIO . forkIO $ actuate network

  liftIO $ do
    -- TODO Move watch to reactive
    killWatch <- Gtk.uiTask watch (\WMCtlMsg -> Gtk.widgetActivate ctrlButton)
    on ctrlButton #destroy killWatch
  pure ctrlButton
  where
    onBuild = do
      (srcFin, srcLine) <- liftIO runBuild
      finished <- fromAddHandler srcFin
      gotLine <- fromAddHandler srcLine
      tab <- stepper Building (Main <$ finished)
      -- For now, do not update back to mempty right after finished
      buildTxt <- accumB mempty (appendLine <$> gotLine)
      pure (tab, buildTxt)

    onRefresh = safeSpawn "xmonad" ["--restart"]

    appendLine line = (<> line <> T.pack "\n")

runBuild :: IO (Source (), Source T.Text)
runBuild = do
  (srcFin, finish) <- sourceSink
  (srcLine, rcvLine) <- sourceSink
  (srcFin, srcLine) <$ go finish rcvLine
  where
    -- MAYBE maybe simplify this?
    go finish rcvLine = do
      forkIO . (`finally` onEnd) . handle @IOException onError $ do
        let prog = proc "xmonad-manage" ["build", "pulpmonad"]
        -- Creates pipe for merging streams
        bracket createPipe (\(r, w) -> hClose r >> hClose w) $ \(reads, writes) -> do
          withCreateProcess prog{std_out = UseHandle writes, std_err = UseHandle writes} $
            \_ _ _ _ -> do
              actOnLine reads rcvLine
      where
        onEnd = threadDelay 3000000 >> finish ()
        onError err = do
          rcvLine (T.pack $ show err)
          throwIO err

        actOnLine outp act = fix $ \recurse ->
          hIsEOF outp >>= \case
            True -> pure ()
            False -> (T.hGetLine outp >>= act) >> recurse

{-------------------------------------------------------------------
                              View
--------------------------------------------------------------------}

data WCTab = Main | Building

data View = View
  { ctrlButton :: !Gtk.Widget
  , window :: !Gtk.Window
  , setTab :: Sink WCTab
  , setBuildText :: Sink T.Text
  , toBuild :: Source ()
  , toRefresh :: Source ()
  }

view :: T.Text -> Gtk.Window -> IO View
view uiFile parent = Gtk.buildFromFile uiFile $ do
  Just ctrlButton <- Gtk.getElement (T.pack "btn-wmctl") Gtk.Widget

  Just window <- Gtk.getElement (T.pack "wmctl") Gtk.Window
  Just stack <- Gtk.getElement (T.pack "wmctl-stack") Gtk.Stack
  Just buildLab <- Gtk.getElement (T.pack "wmctl-build-label") Gtk.Label
  Just buildScr <- Gtk.getElement (T.pack "wmctl-build-scroll") Gtk.ScrolledWindow
  buildAdj <- get buildScr #vadjustment

  set window [#transientFor := parent]
  Gtk.windowSetTransparent window
  on window #deleteEvent $ \_ -> #hideOnDelete window

  let setTab = \case
        Main -> do
          set buildLab [#label := T.empty]
          set stack [#visibleChildName := T.pack "main"]
        Building -> set stack [#visibleChildName := T.pack "build"]

      setBuildText txt = do
        set buildLab [#label := txt]
        set buildAdj [#value :=> get buildAdj #upper]

  (toBuild, build) <- liftIO sourceSink
  (toRefresh, refresh) <- liftIO sourceSink
  Gtk.addCallback (T.pack "wmctl-open") $ #showAll window
  Gtk.addCallback (T.pack "wmctl-close") $ #close window
  Gtk.addCallback (T.pack "wmctl-build") $ build ()
  Gtk.addCallback (T.pack "wmctl-refresh") $ refresh () *> #close window

  pure View{..}

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
