{-# LANGUAGE OverloadedLabels #-}

module System.Applet.WMCtrl (wmCtrlBtn) where

import Control.Concurrent
import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.Foldable
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.GI.Base.Signals
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GI.Gtk.Objects.Button qualified as Gtk
import GI.Gtk.Objects.Frame qualified as Gtk
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
import System.IO
import System.Process
import View.Boxes qualified as View
import View.Imagery qualified as View
import XMonad.Util.Run (safeSpawn)

-- | Window manager control button.
-- Shows the system control dialog.
wmCtrlBtn :: (MonadIO m, MonadXHand m) => Gtk.Window -> m Gtk.Widget
wmCtrlBtn parent = do
  watch <- runXHand wmCtrlListen
  icon <- View.imageStaticNew Gtk.IconSizeLargeToolbar True $ View.ImgSName (T.pack "system-settings-symbolic")
  wid <- Gtk.buttonNewWith (Just icon) (liftIO $ showCtlWin parent)
  liftIO $ do
    killWatch <- Gtk.uiTask watch (\WMCtlMsg -> Gtk.widgetActivate wid)
    on wid #destroy killWatch
  pure wid

data SysCtl = Cancel | Build | Refresh
  deriving (Eq, Ord, Enum, Bounded, Show)

nameOf :: SysCtl -> T.Text
nameOf = \case
  Cancel -> T.pack "Cancel"
  Build -> T.pack "Build"
  Refresh -> T.pack "Refresh"

iconOf :: SysCtl -> T.Text
iconOf = \case
  Cancel -> T.pack "window-close-symbolic"
  Build -> T.pack "document-save-symbolic"
  Refresh -> T.pack "view-refresh-symbolic"

styleOf :: SysCtl -> T.Text
styleOf = \case
  Cancel -> T.pack "btn-ctl"
  Build -> T.pack "btn-ctl"
  Refresh -> T.pack "btn-ctl"

showCtlWin :: Gtk.Window -> IO ()
showCtlWin parent = do
  view@CtlWinView{ctlWin} <- ctlWinNew parent
  ctlWinBuildmode view False

  ctlWinSetAct view $ \case
    Cancel -> #close ctlWin
    Build -> runBuild view
    Refresh -> do
      safeSpawn "xmonad" ["--restart"]
      #close ctlWin
  where
    runBuild :: CtlWinView -> IO ()
    runBuild view = do
      Gtk.uiSingleRun (ctlWinBuildmode view True)
      -- Problem: stdout from the process is captured last, what is happening?
      forkIO . withCreateProcess (proc "xmonad-manage" ["build", "pulpmonad"]){std_out = CreatePipe} $
        \_ (Just outp) _ _ -> do
          actOnLine outp $ \txt -> Gtk.uiSingleRun (ctlWinBuildAddLine view txt)
          Gtk.uiSingleRun (ctlWinBuildmode view False)
      pure ()
      where
        actOnLine outp act =
          hIsEOF outp >>= \case
            True -> pure ()
            False -> do
              line <- T.hGetLine outp
              T.putStrLn (T.pack "::" <> line)
              act line
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

data CtlWinView = CtlWinView
  { ctlWin :: !Gtk.Window
  , ctlStack :: !Gtk.Stack
  , ctlMain :: !CtlMainView
  , ctlBuild :: !CtlBuildView
  }

ctlWinNew :: Gtk.Window -> IO CtlWinView
ctlWinNew parent = do
  ctlWin <-
    new
      Gtk.Window
      [ #title := T.pack "Pulp Manager Control"
      , #typeHint := Gtk.WindowTypeHintDialog
      , #windowPosition := Gtk.WindowPositionCenter
      , #transientFor := parent
      ]
  #setDefaultSize ctlWin 360 140
  #setIconName ctlWin (Just $ T.pack "system-settings")
  Gtk.windowSetTransparent ctlWin

  ctlMain <- ctlMainNew
  ctlBuild <- ctlBuildNew

  ctlStack <- new Gtk.Stack [#homogeneous := True]
  #setName (ctlMainWid ctlMain) (T.pack "pulp-wmctl")
  #setName (ctlBuildWid ctlBuild) (T.pack "pulp-wmctl")
  #addNamed ctlStack (ctlMainWid ctlMain) (T.pack "main")
  #addNamed ctlStack (ctlBuildWid ctlBuild) (T.pack "build")

  #add ctlWin ctlStack
  #showAll ctlWin

  pure CtlWinView{..}

ctlWinBuildmode :: CtlWinView -> Bool -> IO ()
ctlWinBuildmode CtlWinView{..} = \case
  False -> do
    ctlBuildClear ctlBuild -- Clears the build log
    set ctlStack [#visibleChildName := T.pack "main"]
  True -> set ctlStack [#visibleChildName := T.pack "build"]

ctlWinSetAct :: CtlWinView -> (SysCtl -> IO ()) -> IO ()
ctlWinSetAct CtlWinView{ctlMain} = ctlMainSetAct ctlMain

ctlWinBuildAddLine :: CtlWinView -> T.Text -> IO ()
ctlWinBuildAddLine CtlWinView{ctlBuild} = ctlBuildAddLine ctlBuild

data CtlMainView = CtlMainView
  { ctlMainWid :: !Gtk.Widget
  , ctlMainBtns :: !(M.Map SysCtl Gtk.Button)
  }

ctlMainNew :: IO CtlMainView
ctlMainNew = do
  let keys = [minBound .. maxBound]
  btns <- traverse btnFor keys
  let ctlMainBtns = M.fromAscList (zip keys btns)

  box <-
    View.boxStaticNew
      (View.defBoxArg Gtk.OrientationHorizontal)
        { View.boxSpacing = 10
        , View.boxHomogeneous = True
        }
      =<< traverse Gtk.toWidget btns
  #getStyleContext box >>= flip #addClass (T.pack "btn-area")

  ctlMainWid <- Gtk.toWidget box
  #setHalign ctlMainWid Gtk.AlignFill
  #showAll ctlMainWid
  pure CtlMainView{..}
  where
    btnFor ctl = do
      box <-
        View.boxStaticNew (View.defBoxArg Gtk.OrientationVertical){View.boxSpacing = 5}
          =<< sequenceA
            [ View.imageStaticNew Gtk.IconSizeDialog True (View.ImgSName $ iconOf ctl)
            , Gtk.toWidget =<< new Gtk.Label [#label := nameOf ctl]
            ]
      #setValign box Gtk.AlignCenter

      btn <- new Gtk.Button []
      #add btn box
      #getStyleContext btn >>= flip #addClass (styleOf ctl)
      pure btn

ctlMainSetAct :: CtlMainView -> (SysCtl -> IO ()) -> IO ()
ctlMainSetAct CtlMainView{ctlMainBtns} act =
  for_ (M.toList ctlMainBtns) $ \(ctl, btn) -> on btn #clicked (act ctl)

data CtlBuildView = CtlBuildView
  { ctlBuildWid :: !Gtk.Widget
  , ctlBuildLab :: !Gtk.Label
  , ctlBuildScroll :: !Gtk.ScrolledWindow
  }

ctlBuildNew :: IO CtlBuildView
ctlBuildNew = do
  ctlBuildLab <-
    new
      Gtk.Label
      [ #xalign := 0
      , #maxWidthChars := 50
      , #ellipsize := toEnum 3 -- ellipsize at end
      , #label := T.empty
      ]

  ctlBuildScroll <-
    new
      Gtk.ScrolledWindow
      [ #hscrollbarPolicy := Gtk.PolicyTypeNever
      , #vscrollbarPolicy := Gtk.PolicyTypeAlways
      ]
  #add ctlBuildScroll ctlBuildLab

  frame <- new Gtk.Frame []
  #add frame ctlBuildScroll
  ctlBuildWid <- Gtk.toWidget frame

  #showAll ctlBuildWid
  pure CtlBuildView{..}

ctlBuildAddLine :: CtlBuildView -> T.Text -> IO ()
ctlBuildAddLine CtlBuildView{..} lbl = do
  set ctlBuildLab [#label :~ (<> lbl <> T.pack "\n")]
  adj <- get ctlBuildScroll #vadjustment
  set adj [#value :=> get adj #upper]

ctlBuildClear :: CtlBuildView -> IO ()
ctlBuildClear CtlBuildView{ctlBuildLab} = set ctlBuildLab [#label := T.empty]
