module GtkCommons
  ( iconNewFromName,
    iconNewChanneling,
    iconNewTask,
    buttonNewWith,
    overlayed,
    barNewTask,
    imageNew,
    module GI.Gtk.Enums,
    module GI.Gtk.Objects.Widget,
    module GI.Gtk.Objects.Container,
    module GI.Gtk.Objects.IconTheme,
    module GI.Gtk.Objects.StyleContext,
    windowAsTransparent,
    windowGrabOnMap,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Enclosed (tryAny)
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Text qualified as T
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector (renderWithContext)
import GI.GLib.Constants
import GI.GLib.Structs.Source
import GI.Gdk.Flags qualified as Gdk
import GI.Gdk.Functions qualified as Gdk
import GI.Gdk.Objects.Cursor qualified as Gdk
import GI.Gdk.Objects.Display qualified as Gdk
import GI.Gdk.Objects.Screen qualified as Gdk
import GI.Gdk.Objects.Seat qualified as Gdk
import GI.Gdk.Objects.Window qualified as Gdk
import GI.Gdk.Structs.EventAny qualified as Gdk
import GI.Gtk.Enums
import GI.Gtk.Functions (getCurrentEvent)
import GI.Gtk.Objects.Button
import GI.Gtk.Objects.Container
import GI.Gtk.Objects.IconTheme
import GI.Gtk.Objects.Image
import GI.Gtk.Objects.Overlay
import GI.Gtk.Objects.StyleContext
import GI.Gtk.Objects.Widget
import GI.Gtk.Objects.Window
import Task
import XMonad.StackSet (RationalRect (..))

-- TODO
-- Taffybar is suboptimal, esp. dependency. Cook one up myself?

-- | Adds GDK single-use task, which only runs once.
gdkSingleRun :: IO a -> IO ()
gdkSingleRun task = () <$ Gdk.threadsAddIdle PRIORITY_DEFAULT_IDLE (False <$ task)

-- | Adds GDK Task, returns the kill action.
gdkTask :: Task a -> (a -> IO b) -> IO (IO ())
gdkTask (Task kill var) actWith = do
  let action = True <$ (tryTakeMVar var >>= traverse_ actWith)
  sourceId <- Gdk.threadsAddIdle PRIORITY_DEFAULT_IDLE action
  pure $ kill <* sourceRemove sourceId

windowAsTransparent :: MonadIO m => Window -> m ()
windowAsTransparent window = do
  setWidgetAppPaintable window True
  screen <- windowGetScreen window
  composited <- Gdk.screenIsComposited screen
  when composited $
    Gdk.screenGetRgbaVisual screen >>= widgetSetVisual window

-- | Grab the screen on window map
windowGrabOnMap :: MonadIO m => Window -> m ()
windowGrabOnMap window =
  void $
    afterWidgetMapEvent window $
      Gdk.getEventAnyWindow >=> \case
        Nothing -> pure False
        Just win -> do
          event <- getCurrentEvent
          seat <- Gdk.windowGetDisplay win >>= Gdk.displayGetDefaultSeat
          Gdk.seatGrab seat win [Gdk.SeatCapabilitiesAll] True (Nothing @Gdk.Cursor) event Nothing
          pure False

iconNewWith :: MonadIO m => IconSize -> ((T.Text -> IO ()) -> IO a) -> (a -> IO ()) -> m Widget
iconNewWith iconSize realize unrealize = do
  image <- imageNew
  _ <- onWidgetRealize image $ do
    let sizeEnum = fromIntegral $ fromEnum iconSize
    key <- realize $ \name -> imageSetFromIconName image (Just name) sizeEnum
    () <$ onWidgetUnrealize image (unrealize key)
  toWidget image

iconNewFromName :: MonadIO m => IconSize -> T.Text -> m Widget
iconNewFromName iconSize name = iconNewWith iconSize (\setIcon -> setIcon name) pure

iconNewChanneling :: MonadIO m => IconSize -> IO (IO a) -> IO T.Text -> m Widget
iconNewChanneling iconSize mkWaiter getName = iconNewWith iconSize realize killThread
  where
    realize setIcon = do
      waits <- mkWaiter
      forkIO . forever $ do
        tryAny $ getName >>= gdkSingleRun . setIcon
        waits

iconNewTask :: MonadIO m => IconSize -> Task a -> (a -> T.Text) -> m Widget
iconNewTask iconSize task naming = iconNewWith iconSize realize unrealize
  where
    realize setIcon = gdkTask task (setIcon . naming)
    unrealize kill = kill

buttonNewWith :: MonadIO m => Maybe Widget -> IO () -> m Widget
buttonNewWith widget onClick = do
  btn <- buttonNew
  traverse_ (containerAdd btn) widget
  onButtonClicked btn onClick
  toWidget btn

overlayed :: MonadIO m => Widget -> [Widget] -> m Widget
overlayed core overlays = do
  overlay <- overlayNew
  containerAdd overlay core
  traverse_ (overlayAddOverlay overlay) overlays
  -- Pass through inputs
  traverse_ (\wid -> overlaySetOverlayPassThrough overlay wid True) overlays
  toWidget overlay

barNewTask :: MonadIO m => RationalRect -> (Double, Double, Double) -> Task a -> (a -> Double) -> m Widget
barNewTask relative color task@(Task _ var) asFill = do
  -- Uses images as base and render on the image.
  bar <- imageNew
  setWidgetHalign bar AlignFill
  setWidgetValign bar AlignFill
  -- Uses TVar as well to allow rendering
  _ <- onWidgetRealize bar $ do
    tvar <- takeMVar var >>= atomically . newTVar
    killTask <- gdkTask task $ \val ->
      atomically (writeTVar tvar val) *> widgetQueueDraw bar
    _ <- onWidgetDraw bar $ \ctx -> do
      fill <- asFill <$> readTVarIO tvar
      True <$ renderWithContext (drawBar bar relative color fill) ctx
    () <$ onWidgetUnrealize bar killTask
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
