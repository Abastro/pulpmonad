{-# LANGUAGE OverloadedLabels #-}

-- More complicated since GTK is trying to push wayland and wayland is..meh
module Pulp.Desk.UI.Window (
  module GI.Gtk.Objects.Window,
  appWindowNew,
  windowSetTransparent,
  windowGrabOnMap,
  DockPos (..),
  DockSize (..),
  DockSpan (..),
  DockArg (..),
  windowSetDock,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Int
import Data.Vector qualified as V
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import GI.GLib
import GI.GLib qualified as Glib
import GI.Gdk.Flags qualified as Gdk
import GI.Gdk.Objects.Cursor qualified as Gdk
import GI.Gdk.Objects.Display qualified as Gdk
import GI.Gdk.Objects.Window qualified as Gdk
import GI.Gdk.Structs.EventAny qualified as Gdk
import GI.Gdk.Structs.Rectangle qualified as Gdk
import GI.GdkX11.Objects.X11Display qualified as GdkX11
import GI.GdkX11.Objects.X11Window qualified as GdkX11
import GI.Gtk.Functions
import GI.Gtk.Objects.Application
import GI.Gtk.Objects.ApplicationWindow
import GI.Gtk.Objects.Window
import Graphics.X11.Xlib qualified as X11
import Graphics.X11.Xlib.Extras qualified as X11
import Pulp.Desk.UI.Commons
import XMonad (Rectangle (..), scaleRationalRect)
import XMonad.StackSet (RationalRect (..))

-- | Create window for an application.
appWindowNew :: MonadIO m => Application -> m Window
appWindowNew app = applicationWindowNew app >>= toWindow

-- | Make window as transparent.
windowSetTransparent :: MonadIO m => Window -> m ()
windowSetTransparent window = do
  screen <- get window #screen
  composited <- screen.isComposited
  when composited $
    screen.getRgbaVisual >>= window.setVisual

-- | Grab the screen on window map.
windowGrabOnMap :: MonadIO m => Window -> m ()
windowGrabOnMap window = do
  -- Upgrade blocker to GTK4.
  -- Possible Workaroud:
  -- 1. Use X11
  -- 2. Delegate to WM (NOTE: Simple grab should not be done, does not redirect)
  after window #mapEvent $
    Gdk.getEventAnyWindow >=> \case
      Nothing -> pure False
      Just win -> do
        event <- getCurrentEvent
        display <- win.getDisplay
        seat <- display.getDefaultSeat
        seat.grab win [Gdk.SeatCapabilitiesAll] True (Nothing @Gdk.Cursor) event Nothing
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

data DockArg = DockArg
  { dockPos :: !DockPos
  , dockSize :: !DockSize
  , dockSpan :: !DockSpan
  , dockMonitor :: !(Maybe Int32)
  }

-- | Set window as a dock on the primary monitor.
--
-- Assumes the provided size is enough, at least on the strut side.
--
-- Can fail when there is no default display / primary montior.
--
-- NB: Input is not checked
windowSetDock :: (MonadIO m) => Window -> DockArg -> m ()
windowSetDock window DockArg{..} = do
  -- TODO Allow not applying the strut.
  display <- liftIO $ maybe (fail "No default display") pure =<< Gdk.displayGetDefault
  monitor <-
    liftIO $
      maybe (fail "No primary monitor") pure =<< do
        maybe display.getPrimaryMonitor display.getMonitor dockMonitor
  screen <- display.getDefaultScreen

  monRect <- fromGdkRect =<< monitor.getGeometry
  scaleFactor <- monitor.getScaleFactor
  let (fullSize, fullSpan) = fullSizeSpan monRect dockPos
  let dockSizeRatio = sizeRatio fullSize dockSize
  let Rectangle px py width height = scaleRationalRect monRect $ dockRect dockSizeRatio dockSpan dockPos

  set window [#typeHint := WindowTypeHintDock, #skipPagerHint := True, #skipTaskbarHint := True, #screen := screen]
  window.setSizeRequest (fromIntegral width) (fromIntegral height)

  -- Strut: send over "_NET_WM_STRUT_PARTIAL" on map event
  let sizeEl = (fromEnum dockPos, scaleTo fullSize dockSizeRatio)
  let beginEl = (4 + 2 * fromEnum dockPos, scaleTo fullSpan $ dockSpan.dockBegin)
  let endEl = (4 + 2 * fromEnum dockPos + 1, scaleTo fullSpan $ dockSpan.dockEnd)
  let strutVec = (scaleFactor *) <$> V.replicate 12 0 V.// [sizeEl, beginEl, endEl]

  -- Note: On Gtk4, getting surface is the first
  after window #mapEvent $
    Gdk.getEventAnyWindow >=> \case
      Nothing -> pure False
      Just gdkWin -> do
        withXDisplay display $ \xDisplay -> withXWindow gdkWin $ \xWindow -> do
          -- Window positioning
          let scPos = scaleFactor
          let scDim = fromIntegral scaleFactor
          X11.moveResizeWindow xDisplay xWindow (px * scPos) (py * scPos) (width * scDim) (height * scDim)
          -- Strut setting
          let dat :: [CLong] = fromIntegral <$> V.toList strutVec
          strutPartial <- X11.internAtom xDisplay "_NET_WM_STRUT_PARTIAL" False
          typCardinal <- X11.internAtom xDisplay "CARDINAL" False
          X11.changeProperty32 xDisplay xWindow strutPartial typCardinal X11.propModeReplace dat
        pure False
  pure ()
  where
    fromGdkRect :: MonadIO m => Gdk.Rectangle -> m Rectangle
    fromGdkRect rect =
      Rectangle
        <$> get rect #x
        <*> get rect #y
        <*> (fromIntegral <$> get rect #width)
        <*> (fromIntegral <$> get rect #height)

withXDisplay :: Gdk.Display -> (X11.Display -> IO ()) -> IO ()
withXDisplay gdkDisp act = void . runMaybeT $ do
  x11Disp <- MaybeT $ Glib.castTo GdkX11.X11Display gdkDisp
  dispPtr <- x11Disp.getXdisplay
  liftIO . withManagedPtr dispPtr $ \ptr -> act (X11.Display $ castPtr ptr)

withXWindow :: Gdk.Window -> (X11.Window -> IO ()) -> IO ()
withXWindow gdkWin act = void . runMaybeT $ do
  x11Win <- MaybeT $ Glib.castTo GdkX11.X11Window gdkWin
  winID <- x11Win.getXid
  liftIO $ act (fromIntegral winID)
