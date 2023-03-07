{-# LANGUAGE OverloadedLabels #-}

module Pulp.Desk.Applet.Layout (LayoutArg (..), layout) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.GI.Base.Attributes qualified as GI
import Data.Text qualified as T
import GI.Gdk.Unions.Event qualified as Gdk
import GI.Gtk.Objects.Label qualified as Gtk
import Pulp.Desk.Env.PulpEnv
import Pulp.Desk.PulpPath
import Pulp.Desk.Reactive.Entry
import Pulp.Desk.System.X11.WMStatus
import Pulp.Desk.System.X11.XHandle
import Pulp.Desk.UI.Commons qualified as Gtk
import Pulp.Desk.UI.Reactive qualified as Gtk
import Reactive.Banana.Frameworks

newtype LayoutArg = LayoutArg
  { layoutPrettyName :: T.Text -> T.Text
  }

-- | Applet showing current window layout.
layout :: LayoutArg -> PulpIO Gtk.Widget
layout LayoutArg{..} = withRunInIO $ \unlift -> do
  LayoutComm{..} <- unlift $ runXHook layoutInitiate
  uiFile <- dataPath ("ui" </> "layout.ui")
  View{..} <- view (T.pack uiFile)
  let onLayout layout = setLabel (layoutPrettyName layout)

  network <- compile $ do
    eClick <- sourceEvent clicks
    bLayout <- stepsBehavior curLayout

    reactimate (reqToLayout . clickReq <$> eClick)
    syncBehavior bLayout (Gtk.uiSingleRun . onLayout)
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

  let setLabel lbl = GI.set layoutLbl [#label GI.:= lbl]

  (clicks, callClick) <- liftIO sourceSink
  Gtk.addCallbackWithEvent (T.pack "layout-action") Gdk.getEventButton (onAct callClick)

  pure View{..}
  where
    onAct call event =
      GI.get event #button >>= \case
        1 -> call LeftClick
        3 -> call RightClick
        _ -> pure ()

{-------------------------------------------------------------------
                          Communication
--------------------------------------------------------------------}

data LayoutComm = LayoutComm
  { curLayout :: Steps T.Text
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
