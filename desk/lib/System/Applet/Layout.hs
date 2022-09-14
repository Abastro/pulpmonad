{-# LANGUAGE OverloadedLabels #-}

module System.Applet.Layout (LayoutArg (..), layout) where

import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.GI.Base.Attributes
import Data.GI.Base.Signals
import Data.Text qualified as T
import GI.Gdk.Unions.Event qualified as Gdk
import GI.Gtk.Functions qualified as Gtk
import GI.Gtk.Objects.Label qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Task qualified as Gtk
import Status.X11.WMStatus
import Status.X11.XHandle
import System.FilePath
import System.Pulp.PulpEnv (MonadPulpPath (..))

newtype LayoutArg = LayoutArg
  { layoutPrettyName :: T.Text -> T.Text
  }

-- | Applet showing current window layout.
layout :: (MonadXHand m, MonadPulpPath m) => LayoutArg -> m Gtk.Widget
layout LayoutArg{..} = do
  LayoutRcvs{..} <- runXHand layoutInitiate
  uiFile <- pulpDataPath ("ui" </> "layout.ui")
        
  LayoutView{..} <- liftIO $ layoutViewNew (T.pack uiFile) (onClick reqToLayout)
  liftIO $ do
    killLayout <- Gtk.uiTask curLayout $ \layout -> setLabel (layoutPrettyName layout)
    on layoutWid #destroy killLayout

  pure layoutWid
  where
    onClick req = \case
      LeftClick -> req NextLayout
      RightClick -> req ResetLayout

{-------------------------------------------------------------------
                          Communication
--------------------------------------------------------------------}

data LayoutRcvs = LayoutRcvs
  { curLayout :: Task T.Text
  , reqToLayout :: LayoutCmd -> IO ()
  }

layoutInitiate :: XIO () LayoutRcvs
layoutInitiate = do
  rootWin <- xWindow
  curLayout <- errorAct $ watchXQuery rootWin getDesktopLayout pure
  reqToLayout <- reqDesktopLayout
  pure LayoutRcvs{..}
  where
    onError window err = do
      liftIO (fail $ formatXQError window err)
    errorAct act = do
      window <- xWindow
      act >>= either (onError window) pure

{-------------------------------------------------------------------
                              View
--------------------------------------------------------------------}

data LayoutView = LayoutView
  { layoutWid :: !Gtk.Widget
  , setLabel :: T.Text -> IO ()
  }

data Click = LeftClick | RightClick

layoutViewNew :: T.Text -> (Click -> IO ()) -> IO LayoutView
layoutViewNew uiFile acts = Gtk.buildFromFile uiFile $ do
  Just layoutWid <- Gtk.getElement (T.pack "layout") Gtk.Widget
  Just layoutLbl <- Gtk.getElement (T.pack "layout-current") Gtk.Label

  Gtk.addCallback (T.pack "layout-action") onAct
  let setLabel lbl = set layoutLbl [#label := lbl]
  pure LayoutView{..}
  where
    onAct =
      Gtk.getCurrentEvent >>= \case
        Nothing -> pure ()
        Just event -> do
          evBtn <- Gdk.getEventButton event
          get evBtn #button >>= \case
            1 -> acts LeftClick
            3 -> acts RightClick
            _ -> pure ()
