{-# LANGUAGE OverloadedLabels #-}

module System.Applet.Layout (LayoutArg (..), layout) where

import Control.Concurrent.Task
import Control.Event.Entry
import Control.Monad.IO.Class
import Data.GI.Base.Attributes
import Data.Text qualified as T
import GI.Gdk.Unions.Event qualified as Gdk
import GI.Gtk.Objects.Label qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Task qualified as Gtk
import Reactive.Banana.Frameworks
import Status.X11.WMStatus
import Status.X11.XHandle
import System.FilePath
import System.Pulp.PulpPath
import Control.Monad.IO.Unlift
import System.Pulp.PulpEnv

newtype LayoutArg = LayoutArg
  { layoutPrettyName :: T.Text -> T.Text
  }

-- | Applet showing current window layout.
layout :: LayoutArg -> PulpIO Gtk.Widget
layout LayoutArg{..} = withRunInIO $ \unlift -> do
  LayoutComm{..} <- unlift $ runXHand layoutInitiate
  uiFile <- dataPath ("ui" </> "layout.ui")
  View{..} <- view (T.pack uiFile)
  let onLayout layout = setLabel (layoutPrettyName layout)

  network <- compile $ do
    clickEvent <- sourceEvent clicks
    layoutEvent <- sourceEvent (taskToSource curLayout)

    reactimate (reqToLayout . clickReq <$> clickEvent)
    reactimate (Gtk.uiSingleRun . onLayout <$> layoutEvent)
  actuate network

  pure layoutWid
  where
    clickReq = \case
      LeftClick -> NextLayout
      RightClick -> ResetLayout

{-------------------------------------------------------------------
                              View
--------------------------------------------------------------------}

data View = View
  { layoutWid :: !Gtk.Widget
  , setLabel :: Sink T.Text
  , clicks :: Source Click
  }

data Click = LeftClick | RightClick

view :: T.Text -> IO View
view uiFile = Gtk.buildFromFile uiFile $ do
  Just layoutWid <- Gtk.getElement (T.pack "layout") Gtk.Widget
  Just layoutLbl <- Gtk.getElement (T.pack "layout-current") Gtk.Label

  let setLabel lbl = set layoutLbl [#label := lbl]

  (clicks, callClick) <- liftIO sourceSink
  Gtk.addCallbackWithEvent (T.pack "layout-action") Gdk.getEventButton (onAct callClick)

  pure View{..}
  where
    onAct call event =
      get event #button >>= \case
        1 -> call LeftClick
        3 -> call RightClick
        _ -> pure ()

{-------------------------------------------------------------------
                          Communication
--------------------------------------------------------------------}

data LayoutComm = LayoutComm
  { curLayout :: Task T.Text
  , reqToLayout :: Sink LayoutCmd
  }

layoutInitiate :: XIO LayoutComm
layoutInitiate = do
  rootWin <- xWindow
  curLayout <- errorAct $ watchXQuery rootWin getDesktopLayout pure
  reqToLayout <- reqDesktopLayout
  pure LayoutComm{..}
  where
    onError window err = do
      liftIO (fail $ formatXQError window err)
    errorAct act = do
      window <- xWindow
      act >>= either (onError window) pure
