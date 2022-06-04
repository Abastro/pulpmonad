module UI.Singles (
  module GI.Gtk.Objects.Label,
  imageNew,
  iconNewFromName,
  iconNewTask,
  barNewTask,
) where

import Control.Concurrent.STM
import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.Text qualified as T
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector
import GI.Gtk.Objects.Image
import UI.Commons
import UI.Task
import GI.Gtk.Objects.Label
import XMonad.StackSet (RationalRect (..))

iconNewWith :: MonadIO m => IconSize -> ((T.Text -> IO ()) -> IO a) -> (a -> IO ()) -> m Widget
iconNewWith iconSize realize unrealize = do
  image <- imageNew
  _ <- onWidgetRealize image $ do
    let sizeEnum = fromIntegral $ fromEnum iconSize
    key <- realize $ \name -> imageSetFromIconName image (Just name) sizeEnum
    onWidgetUnrealize image (unrealize key)
    pure ()
  toWidget image

iconNewFromName :: MonadIO m => IconSize -> T.Text -> m Widget
iconNewFromName iconSize name = iconNewWith iconSize (\setIcon -> setIcon name) pure

iconNewTask :: MonadIO m => IconSize -> Task a -> (a -> T.Text) -> m Widget
iconNewTask iconSize task naming = iconNewWith iconSize realize unrealize
  where
    realize setIcon = uiTask task (setIcon . naming)
    unrealize kill = kill

barNewTask :: MonadIO m => RationalRect -> (Double, Double, Double) -> Task a -> (a -> Double) -> m Widget
barNewTask relative color task asFill = do
  -- Uses images as base and render on the image.
  bar <- imageNew
  setWidgetHalign bar AlignFill
  setWidgetValign bar AlignFill
  -- Uses TVar as well to allow rendering
  _ <- onWidgetRealize bar $ do
    -- MAYBE avoid blocking?
    tvar <- taskNextWait task >>= newTVarIO
    killTask <- uiTask task $ \val ->
      atomically (writeTVar tvar val) *> widgetQueueDraw bar
    _ <- onWidgetDraw bar $ \ctx -> do
      fill <- asFill <$> readTVarIO tvar
      True <$ renderWithContext (drawBar bar relative color fill) ctx
    onWidgetUnrealize bar killTask
    pure ()
  toWidget bar

-- For now, assume from down to up
drawBar :: Image -> RationalRect -> (Double, Double, Double) -> Double -> C.Render ()
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
