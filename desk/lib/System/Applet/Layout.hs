{-# LANGUAGE OverloadedLabels #-}

module System.Applet.Layout (LayoutArg (..), layout) where

import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.Text qualified as T
import GI.Gtk.Objects.Label qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Containers qualified as Gtk
import Gtk.Task qualified as Gtk
import Status.X11.WMStatus
import Status.X11.XHandle

newtype LayoutArg = LayoutArg
  { layoutPrettyName :: T.Text -> T.Text
  }

-- | Applet showing current window layout.
layout :: (MonadXHand m) => LayoutArg -> m Gtk.Widget
layout arg = do
  rcvs <- runXHand layoutInitiate
  view <- layoutViewNew
  LayoutHandle <- layoutMake arg rcvs view
  pure $ layoutWid view

-- Dividing into multiple files would be unnecessary hassle.

{-------------------------------------------------------------------
                              Handle
--------------------------------------------------------------------}

data LayoutHandle = LayoutHandle

layoutMake :: MonadIO m => LayoutArg -> LayoutRcvs -> LayoutView -> m LayoutHandle
layoutMake LayoutArg{..} LayoutRcvs{..} view = liftIO registers
  where
    registers = do
      layoutSetAction view (reqToLayout NextLayout) (reqToLayout ResetLayout)
      killLayout <- Gtk.uiTask curLayout updateLayout
      _ <- Gtk.onWidgetDestroy (layoutWid view) killLayout
      pure LayoutHandle
      where
        updateLayout layout = do
          layoutSetLabel view (layoutPrettyName layout)

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
  , layoutLbl :: !Gtk.Label
  }

layoutViewNew :: MonadIO m => m LayoutView
layoutViewNew = do
  layoutLbl <- new Gtk.Label []
  layLblWid <- Gtk.toWidget layoutLbl
  -- Button to show the decoration
  btn <- Gtk.buttonNewWith (Just layLblWid) $ pure ()

  layoutWid <- Gtk.toWidget btn
  Gtk.widgetSetName layoutWid (T.pack "window-layout")
  pure LayoutView{..}

layoutSetAction :: MonadIO m => LayoutView -> IO () -> IO () -> m ()
layoutSetAction LayoutView{layoutWid} leftClick rightClick = do
  Gtk.onWidgetButtonReleaseEvent layoutWid $ \event -> do
    get event #button >>= \case
      1 -> True <$ leftClick
      3 -> True <$ rightClick
      _ -> pure False
  pure ()

layoutSetLabel :: MonadIO m => LayoutView -> T.Text -> m ()
layoutSetLabel LayoutView{layoutLbl} lbl = set layoutLbl [#label := lbl]
