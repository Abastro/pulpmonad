{-# LANGUAGE MonoLocalBinds #-}

module UI.Window (
  module GI.Gtk.Objects.Window,
  appWindowNew,
  windowSetTransparent,
  windowGrabOnMap,
  DockPos(..),
  DockSize(..),
  DockSpan(..),
  windowSetDock,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import Data.Vector qualified as V
import Data.Word
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import GI.GLib
import GI.Gdk.Flags qualified as Gdk
import GI.Gdk.Objects.Cursor qualified as Gdk
import GI.Gdk.Objects.Display qualified as Gdk
import GI.Gdk.Objects.Monitor qualified as Gdk
import GI.Gdk.Objects.Screen qualified as Gdk
import GI.Gdk.Objects.Seat qualified as Gdk
import GI.Gdk.Objects.Window qualified as Gdk
import GI.Gdk.Structs.Atom qualified as Gdk
import GI.Gdk.Structs.EventAny qualified as Gdk
import GI.Gdk.Structs.Rectangle qualified as Gdk
import GI.Gtk.Functions
import GI.Gtk.Objects.Application
import GI.Gtk.Objects.ApplicationWindow
import GI.Gtk.Objects.Window
import UI.Commons
import XMonad (Rectangle (..), scaleRationalRect)
import XMonad.StackSet (RationalRect (..))
import qualified Data.Text as T

-- | Create window for an application.
appWindowNew :: MonadIO m => Application -> m Window
appWindowNew app = applicationWindowNew app >>= toWindow

-- | Make window as transparent.
windowSetTransparent :: MonadIO m => Window -> m ()
windowSetTransparent window = do
  setWidgetAppPaintable window True
  screen <- windowGetScreen window
  composited <- Gdk.screenIsComposited screen
  when composited $
    Gdk.screenGetRgbaVisual screen >>= widgetSetVisual window

-- | Grab the screen on window map.
windowGrabOnMap :: MonadIO m => Window -> m ()
windowGrabOnMap window = do
  afterWidgetMapEvent window $
    Gdk.getEventAnyWindow >=> \case
      Nothing -> pure False
      Just win -> do
        event <- getCurrentEvent
        seat <- Gdk.windowGetDisplay win >>= Gdk.displayGetDefaultSeat
        Gdk.seatGrab seat win [Gdk.SeatCapabilitiesAll] True (Nothing @Gdk.Cursor) event Nothing
        pure False
  pure ()

-- | Dock position. Ordered to match that of "_NET_WM_STRUT_PARTIAL"
data DockPos = DockLeft | DockRight | DockTop | DockBottom
  deriving (Show, Enum)
data DockSize = AbsoluteSize !Word32 | RelativeSize !Rational
  deriving (Show)
data DockSpan = DockSpan {dockBegin :: !Rational, dockEnd :: !Rational}
  deriving (Show)

sizeRatio :: Word32 -> DockSize -> Rational
sizeRatio full = \case
  RelativeSize rel -> rel
  AbsoluteSize size -> max 0 . min 1 $ fromIntegral size / fromIntegral full

-- | Tuple of full size (perpendicular to DockPos) and full span (parallel to DockPos).
fullSizeSpan :: Rectangle -> DockPos -> (Word32, Word32)
fullSizeSpan Rectangle{..} = \case
  DockLeft -> (rect_width, rect_height)
  DockRight -> (rect_width, rect_height)
  DockTop -> (rect_height, rect_width)
  DockBottom -> (rect_height, rect_width)

dockRect :: Rational -> DockSpan -> DockPos -> RationalRect
dockRect size DockSpan{..} = \case
  DockLeft -> RationalRect 0 dockBegin size dockWidth
  DockRight -> RationalRect (1 - size) dockBegin size dockWidth
  DockTop -> RationalRect dockBegin 0 dockWidth size
  DockBottom -> RationalRect dockBegin (1 - size) dockWidth size
  where
    dockWidth = dockEnd - dockBegin

scaleTo :: Word32 -> Rational -> Int32
scaleTo full ratio = floor $ fromIntegral full * ratio

-- | Set window as a dock on the primary monitor.
-- Assumes the provided size is enough, at least on the strut side.
-- Can fail when there is no default display / primary montior.
--
-- TODO handle placing on the monitor
-- TODO Check input (or not)
-- TODO Perhaps allow not applying the strut
windowSetDock :: (MonadIO m) => Window -> DockPos -> DockSize -> DockSpan -> m ()
windowSetDock window pos size span@DockSpan{..} = do
  windowSetTypeHint window WindowTypeHintDock
  windowSetSkipPagerHint window True
  windowSetSkipTaskbarHint window True

  display <- liftIO $ maybe (fail "No default display") pure =<< Gdk.displayGetDefault
  monitor <- liftIO $ maybe (fail "No primary monitor") pure =<< Gdk.displayGetPrimaryMonitor display
  screen <- Gdk.displayGetDefaultScreen display

  monRect <- fromGdkRect =<< Gdk.monitorGetGeometry monitor
  scaleFactor <- Gdk.monitorGetScaleFactor monitor
  let (fullSize, fullSpan) = fullSizeSpan monRect pos
  let dockSize = sizeRatio fullSize size
  let Rectangle px py width height = scaleRationalRect monRect $ dockRect dockSize span pos

  windowSetScreen window screen
  -- Let's not put hard limit on this. User's responsibility to get the size right :P
  widgetSetSizeRequest window (fromIntegral width) (fromIntegral height)
  windowMove window px py

  -- Strut: send over "_NET_WM_STRUT_PARTIAL" on map event
  let sizeEl = (fromEnum pos, scaleTo fullSize dockSize)
  let beginEl = (4 + 2 * fromEnum pos, scaleTo fullSpan dockBegin)
  let endEl = (4 + 2 * fromEnum pos + 1, scaleTo fullSpan dockEnd)
  let strutVec = (scaleFactor *) <$> V.replicate 12 0 V.// [sizeEl, beginEl, endEl]
  strutPartial <- Gdk.atomIntern (T.pack "_NET_WM_STRUT_PARTIAL") False
  typCardinal <- Gdk.atomIntern (T.pack "CARDINAL") False

  afterWidgetMapEvent window $
    Gdk.getEventAnyWindow >=> \case
      Nothing -> pure False
      Just gdkWin -> do
        let dat :: [CULong] = fromIntegral <$> V.toList strutVec
        False <$ gdkPropertyChange gdkWin strutPartial typCardinal 32 PropModeReplace dat
  pure ()
  where
    fromGdkRect rect =
      Rectangle
        <$> Gdk.getRectangleX rect
        <*> Gdk.getRectangleY rect
        <*> (fromIntegral <$> Gdk.getRectangleWidth rect)
        <*> (fromIntegral <$> Gdk.getRectangleHeight rect)

-- | Change gdk property. This was apparently not exposed to language bindings.
-- Only CUChar, CUShort, CULong could be accepted.
-- 4th argument is format, i.e. char for 8, short for 16, long for 32.
gdkPropertyChange ::
  (Gdk.IsWindow w, MonadIO m, Storable a) =>
  w ->
  Gdk.Atom ->
  Gdk.Atom ->
  Int32 ->
  PropMode ->
  [a] ->
  m ()
gdkPropertyChange win property typ format mode dat = liftIO $ do
  window <- Gdk.toWindow win
  withManagedPtr window $ \window' ->
    withManagedPtr property $ \property' ->
      withManagedPtr typ $ \typ' ->
        withArrayLen dat $ \len ptr -> do
          let mode' = fromIntegral . fromEnum $ mode
          let ptr' = castPtr ptr
          let len' = fromIntegral len
          gdk_property_change window' property' typ' format mode' ptr' len'

-- I dislike how Int32 is liberally used, but I digress.
foreign import ccall "gdk_property_change"
  gdk_property_change ::
    Ptr Gdk.Window -> Ptr Gdk.Atom -> Ptr Gdk.Atom -> Int32 -> CUInt -> Ptr CUChar -> Int32 -> IO ()
