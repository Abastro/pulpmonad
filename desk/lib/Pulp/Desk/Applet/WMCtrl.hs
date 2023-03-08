{-# LANGUAGE OverloadedLabels #-}

module Pulp.Desk.Applet.WMCtrl (wmCtrlBtn) where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Unlift
import Data.Function hiding (on)
import Data.GI.Base.Attributes qualified as GI
import Data.GI.Base.Signals qualified as GI
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GI.Gtk.Objects.Label qualified as Gtk
import GI.Gtk.Objects.ScrolledWindow qualified as Gtk
import GI.Gtk.Objects.Stack qualified as Gtk
import Graphics.X11.Types qualified as X11
import Graphics.X11.Xlib.Extras qualified as X11
import Pulp.Desk.Env.PulpEnv
import Pulp.Desk.PulpPath
import Pulp.Desk.Reactive.Entry
import Pulp.Desk.System.X11.XHandle qualified as X11
import Pulp.Desk.UI.Commons qualified as Gtk
import Pulp.Desk.UI.Reactive qualified as Gtk
import Pulp.Desk.UI.Window qualified as Gtk
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import System.IO
import System.Process
import XMonad.Util.Run (safeSpawn)

-- | Window manager control button.
-- Shows the system control dialog.
wmCtrlBtn :: Gtk.Window -> PulpIO Gtk.Widget
wmCtrlBtn parent = withRunInIO $ \unlift -> do
  watch <- unlift $ X11.runXHook wmCtrlListen
  uiFile <- dataPath ("ui" </> "wmctl.ui")
  View{..} <- view (T.pack uiFile) parent

  network <- compile $ do
    callEvent <- sourceEvent watch
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

  GI.set window [#transientFor GI.:= parent]
  Gtk.windowSetTransparent window
  GI.on window #deleteEvent $ \_ -> #hideOnDelete window

  let openWindow = window.showAll

      setTab = \case
        Main -> do
          GI.set buildLab [#label GI.:= T.empty]
          GI.set stack [#visibleChildName GI.:= T.pack "main"]
        Building -> GI.set stack [#visibleChildName GI.:= T.pack "build"]

      setBuildText txt = do
        GI.set buildLab [#label GI.:= txt]
        -- Scroll to the end.
        -- Scrolling is off, likely due to text size not being directly applied.
        -- Maybe switching to TextView would fix this.
        --
        -- On the other hand, current form is not the final go-to visual,
        -- so entire scroll could be removed.
        adj <- GI.get buildScr #vadjustment
        upper <- GI.get adj #upper
        pageSize <- GI.get adj #pageSize
        GI.set adj [#value GI.:= upper - pageSize]

  (toBuild, build) <- liftIO sourceSink
  (toRefresh, refresh) <- liftIO sourceSink
  Gtk.addCallback (T.pack "wmctl-open") openWindow
  Gtk.addCallback (T.pack "wmctl-close") $ window.close
  Gtk.addCallback (T.pack "wmctl-build") $ build ()
  Gtk.addCallback (T.pack "wmctl-refresh") $ refresh () *> window.close

  pure View{..}

{-------------------------------------------------------------------
                          Communication
--------------------------------------------------------------------}

data WMCtrlCall = WMCtrlCall

wmCtrlListen :: X11.XIO (Source WMCtrlCall)
wmCtrlListen = do
  rootWin <- X11.xWindow
  ctrlTyp <- X11.xAtom "_XMONAD_CTRL_MSG"
  ctrlSys <- X11.xAtom "_XMONAD_CTRL_WM"
  X11.xListenSource X11.structureNotifyMask rootWin $ \case
    X11.ClientMessageEvent{ev_message_type = msgTyp, ev_data = subTyp : _}
      | msgTyp == ctrlTyp
      , fromIntegral subTyp == ctrlSys -> do
          pure (Just WMCtrlCall)
    _ -> pure Nothing
