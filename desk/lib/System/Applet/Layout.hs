{-# LANGUAGE OverloadedLabels #-}

module System.Applet.Layout (LayoutArg (..), layout) where

import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.GI.Base.Attributes
import Data.Text qualified as T
import Foreign.Ptr (nullPtr)
import GI.Gdk.Unions.Event qualified as Gdk
import GI.Gtk.Functions qualified as Gtk
import GI.Gtk.Objects.Builder qualified as Gtk
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
layout arg = do
  rcvs <- runXHand layoutInitiate
  uiFile <- pulpDataPath ("ui" </> "layout.ui")

  view <- liftIO $ layoutViewNew (T.pack uiFile)
  LayoutHandle <- layoutMake arg rcvs view
  pure $ layoutWid view

-- Dividing into multiple files would be unnecessary hassle.

{-------------------------------------------------------------------
                              Handle
--------------------------------------------------------------------}

data LayoutHandle = LayoutHandle

layoutMake :: MonadIO m => LayoutArg -> LayoutRcvs -> LayoutView -> m LayoutHandle
layoutMake LayoutArg{..} LayoutRcvs{..} LayoutView{..} = liftIO registers
  where
    registers = do
      setupAct (reqToLayout NextLayout) (reqToLayout ResetLayout)
      killLayout <- Gtk.uiTask curLayout updateLayout
      _ <- Gtk.onWidgetDestroy layoutWid killLayout
      pure LayoutHandle

    updateLayout layout = do
      setLabel (layoutPrettyName layout)

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
  , setupAct :: IO () -> IO () -> IO ()
  }

layoutViewNew :: T.Text -> IO LayoutView
layoutViewNew uiFile = do
  builder <- Gtk.builderNewFromFile uiFile

  Just layoutWid <- Gtk.elementAs builder (T.pack "layout") Gtk.Widget
  Just layoutLbl <- Gtk.elementAs builder (T.pack "layout-current") Gtk.Label

  let setLabel lbl = set layoutLbl [#label := lbl]
      setupAct leftClick rightClick = do
        #addCallbackSymbol builder (T.pack "layout-action") (onAct leftClick rightClick)
        #connectSignals builder nullPtr

  pure LayoutView{..}
  where
    onAct leftClick rightClick =
      Gtk.getCurrentEvent >>= \case
        Nothing -> pure ()
        Just event -> do
          evBtn <- Gdk.getEventButton event
          get evBtn #button >>= \case
            1 -> leftClick
            3 -> rightClick
            _ -> pure ()
