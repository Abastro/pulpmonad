module GtkCommons
  ( iconNewFromName,
    iconNewPolling,
    iconNewChanneling,
    widgetClickable,
    overlayed,
    barNewPolling,
    imageNew,
    module Data.GI.Gtk.Threading,
    module GI.Gtk.Enums,
    module GI.Gtk.Objects.Widget,
    module GI.Gtk.Objects.Container,
    module GI.Gtk.Objects.IconTheme,
    module GI.Gtk.Objects.StyleContext,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Enclosed (tryAny)
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.GI.Gtk.Threading
import Data.Text qualified as T
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector (renderWithContext)
import GI.Gdk.Structs.EventButton (getEventButtonButton)
import GI.Gtk.Enums
import GI.Gtk.Objects.Container
import GI.Gtk.Objects.DrawingArea
import GI.Gtk.Objects.EventBox
import GI.Gtk.Objects.IconTheme
import GI.Gtk.Objects.Image
import GI.Gtk.Objects.Overlay
import GI.Gtk.Objects.StyleContext
import GI.Gtk.Objects.Widget
import XMonad.StackSet (RationalRect (..))

-- TODO
-- 1. These GTK stuffs should be separate from XMonad.
-- Separate build routine? Certainly have enough to handle one myself.
-- 2. Taffybar is suboptimal, esp. dependency. Cook one up myself?

delaySeconds :: Double -> IO ()
delaySeconds interval = threadDelay $ floor (interval * 1000000)

iconNewWith :: MonadIO m => ((T.Text -> IO ()) -> IO a) -> (a -> IO ()) -> m Widget
iconNewWith realize unrealize = do
  image <- imageNew
  _ <- onWidgetRealize image $ do
    let sizeEnum = fromIntegral $ fromEnum IconSizeDnd
    key <- realize $ \name -> imageSetFromIconName image (Just name) sizeEnum
    () <$ onWidgetUnrealize image (unrealize key)
  toWidget image

iconNewFromName :: MonadIO m => T.Text -> m Widget
iconNewFromName name = iconNewWith (\setIcon -> setIcon name) pure

iconNewPolling :: MonadIO m => Double -> IO T.Text -> m Widget
iconNewPolling interval getName = iconNewChanneling (pure $ delaySeconds interval) getName

iconNewChanneling :: MonadIO m => IO (IO a) -> IO T.Text -> m Widget
iconNewChanneling mkWaiter getName = iconNewWith realize killThread
  where
    realize setIcon = do
      waits <- mkWaiter
      forkIO . forever $ do
        tryAny $ getName >>= postGUIASync . setIcon
        waits

widgetClickable :: MonadIO m => Widget -> IO () -> m Widget
widgetClickable widget onClick = do
  ev <- eventBoxNew
  containerAdd ev widget
  onWidgetButtonReleaseEvent ev handleClick
  toWidget ev
  where
    handleClick btn = do
      b <- getEventButtonButton btn
      True <$ when (b == 1) onClick

overlayed :: MonadIO m => Widget -> [Widget] -> m Widget
overlayed core overlays = do
  overlay <- overlayNew
  containerAdd overlay core
  traverse_ (overlayAddOverlay overlay) overlays
  toWidget overlay

barNewPolling :: MonadIO m => RationalRect -> (Double, Double, Double) -> Double -> IO Double -> m Widget
barNewPolling relative color interval getFill = do
  area <- drawingAreaNew
  setWidgetHalign area AlignFill
  setWidgetValign area AlignFill
  _ <- onWidgetRealize area $ do
    fvar <- getFill >>= atomically . newTVar
    -- Updating
    forkIO . forever $ do
      tryAny $ do
        getFill >>= atomically . writeTVar fvar
        postGUIASync $ widgetQueueDraw area
      delaySeconds interval
    -- Rendering
    _ <- onWidgetDraw area $ \ctx -> do
      fill <- readTVarIO fvar
      True <$ renderWithContext (drawBar area relative color fill) ctx
    pure ()
  toWidget area

-- For now, assume from down to up
drawBar :: DrawingArea -> RationalRect -> (Double, Double, Double) -> Double -> C.Render ()
drawBar area relative (red, green, blue) fill = do
  w <- widgetGetAllocatedWidth area
  h <- widgetGetAllocatedHeight area
  let RationalRect l t r d = relative
      left = realToFrac l * realToFrac w
      top = realToFrac t * realToFrac h
      right = realToFrac r * realToFrac w
      down = realToFrac d * realToFrac h
  C.translate left top
  C.scale (right - left) (down - top)
  C.setSourceRGB red green blue
  C.rectangle 0.0 (1.0 - fill) 1.0 fill
  C.fill
