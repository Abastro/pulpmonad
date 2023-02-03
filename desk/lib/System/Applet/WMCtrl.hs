{-# LANGUAGE OverloadedLabels #-}

module System.Applet.WMCtrl (wmCtrlBtn) where

import Control.Concurrent
import Control.Concurrent.Task
import Control.Event.Entry
import Control.Exception
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
import XMonad.Util.Run (safeSpawn)
import System.Pulp.PulpPath
import Control.Monad.IO.Unlift

-- | Window manager control button.
-- Shows the system control dialog.
wmCtrlBtn :: (MonadUnliftIO m, MonadXHand m) => Gtk.Window -> m Gtk.Widget
wmCtrlBtn parent = withRunInIO $ \unlift -> do
  watch <- unlift $ runXHand wmCtrlListen
  uiFile <- dataPath ("ui" </> "wmctl.ui")
  View{..} <- view (T.pack uiFile) parent

  network <- compile $ do
    callEvent <- sourceEvent (taskToSource watch)
    buildEvent <- sourceEvent toBuild
    refreshEvent <- sourceEvent toRefresh

    -- TODO Type-annotate the need of synchronization for GTK actions
    reactimate (Gtk.uiSingleRun openWindow <$ callEvent)
    reactimate (onRefresh <$ refreshEvent)

    builds <- execute (onBuild <$ buildEvent)
    tab <- switchB (pure Main) $ fst <$> builds
    buildTxt <- switchB mempty $ snd <$> builds
    syncBehavior tab $ Gtk.uiSingleRun . setTab
    syncBehavior buildTxt $ Gtk.uiSingleRun . setBuildText
  actuate network -- Not concurrent, "actuate" call only sets a variable

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
  , openWindow :: IO ()
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

  set window [#transientFor := parent]
  Gtk.windowSetTransparent window
  on window #deleteEvent $ \_ -> #hideOnDelete window

  let openWindow = #showAll window

      setTab = \case
        Main -> do
          set buildLab [#label := T.empty]
          set stack [#visibleChildName := T.pack "main"]
        Building -> set stack [#visibleChildName := T.pack "build"]

      setBuildText txt = do
        set buildLab [#label := txt]
        -- Scroll to the end.
        -- Scrolling is off, likely due to text size not being directly applied.
        -- Maybe switching to TextView would fix this.
        --
        -- On the other hand, current form is not the final go-to visual,
        -- so entire scroll could be removed.
        adj <- get buildScr #vadjustment
        upper <- get adj #upper
        pageSize <- get adj #pageSize
        set adj [#value := upper - pageSize]

  (toBuild, build) <- liftIO sourceSink
  (toRefresh, refresh) <- liftIO sourceSink
  Gtk.addCallback (T.pack "wmctl-open") openWindow
  Gtk.addCallback (T.pack "wmctl-close") $ #close window
  Gtk.addCallback (T.pack "wmctl-build") $ build ()
  Gtk.addCallback (T.pack "wmctl-refresh") $ refresh () *> #close window

  pure View{..}

{-------------------------------------------------------------------
                          Communication
--------------------------------------------------------------------}

data WMCtrlCall = WMCtrlCall

wmCtrlListen :: XIO (Task WMCtrlCall)
wmCtrlListen = do
  rootWin <- xWindow
  ctrlTyp <- xAtom "_XMONAD_CTRL_MSG"
  ctrlSys <- xAtom "_XMONAD_CTRL_WM"
  xListenTo structureNotifyMask rootWin Nothing $ \case
    ClientMessageEvent{ev_message_type = msgTyp, ev_data = subTyp : _}
      | msgTyp == ctrlTyp
      , fromIntegral subTyp == ctrlSys -> do
          pure (Just WMCtrlCall)
    _ -> pure Nothing
