-- | Image-like Views.
module View.Imagery (
  ImageSet (..),
  ImageDyn,
  imageDynWidget,
  imageDynNew,
  imageDynSetImg,
  imageStaticNew,
  BarColor (..),
  Bar,
  barWidget,
  barNew,
  barSetFill,
) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import Data.Text qualified as T
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector qualified as C
import GI.Gdk.Structs.RGBA qualified as Gdk
import GI.GdkPixbuf.Objects.Pixbuf qualified as Gdk
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gtk.Objects.Image qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Pixbufs qualified as Gtk
import Gtk.Styles qualified as Gtk
import XMonad.StackSet (RationalRect (..))

-- | Setting image for Image widget
data ImageSet = ImgSName T.Text | ImgSGIcon Gio.Icon | ImgSPixbuf Gdk.Pixbuf

data ImageDyn = ImageDyn
  { imageDynWid :: !Gtk.Widget
  , imageDynSize :: !Int32
  , imageDynImg :: !Gtk.Image
  }

imageDynWidget :: ImageDyn -> Gtk.Widget
imageDynWidget ImageDyn{imageDynWid} = imageDynWid

-- | Create a dynamic image with certain size.
-- If the flag is true, prevents image getting bigger.
imageDynNew :: MonadIO m => Gtk.IconSize -> Bool -> m ImageDyn
imageDynNew size fixSize = do
  imageDynImg <- Gtk.imageNew
  let imageDynSize = fromIntegral $ fromEnum size
  imageDynWid <- Gtk.toWidget imageDynImg
  when fixSize $ Gtk.imageSetPixelSize imageDynImg $ Gtk.iconSizePx size
  pure ImageDyn{..}

imageDynSetImg :: MonadIO m => ImageDyn -> ImageSet -> m ()
imageDynSetImg ImageDyn{..} = \case
  ImgSName txt -> Gtk.imageSetFromIconName imageDynImg (Just txt) imageDynSize
  ImgSGIcon icon -> Gtk.imageSetFromGicon imageDynImg icon imageDynSize
  ImgSPixbuf pix -> Gtk.imageSetFromPixbuf imageDynImg (Just pix)

-- | Static version of 'imageDynNew'.
imageStaticNew :: MonadIO m => Gtk.IconSize -> Bool -> ImageSet -> m Gtk.Widget
imageStaticNew size fixSize sets = do
  dyn <- imageDynNew size fixSize
  imageDynSetImg dyn sets
  pure $ imageDynWidget dyn

data BarColor = BarColor !Double !Double !Double

-- MAYBE Use ProgressBar instead
data Bar = Bar
  { barWid :: !Gtk.Widget
  , barFill :: !(TVar Double)
  }

barWidget :: Bar -> Gtk.Widget
barWidget Bar{barWid} = barWid

-- | Bar with initial fill of 0. Rectangle is left-top-right-bottom.
-- (Required only because impossible to have %-margin in GTK)
barNew :: MonadIO m => Gtk.Orientation -> RationalRect -> m Bar
barNew orient relative = do
  barWid <- Gtk.toWidget =<< Gtk.imageNew -- Image is the most benign
  -- Cannot extend the class from haskell side, soo..
  Gtk.widgetGetStyleContext barWid >>= flip Gtk.styleContextAddClass (T.pack "bar")
  Gtk.setWidgetHalign barWid Gtk.AlignFill
  Gtk.setWidgetValign barWid Gtk.AlignFill
  barFill <- liftIO $ newTVarIO 0.0
  -- Major GTK4 upgrade blocker - need relying on widget class to draw anything
  _ <- Gtk.onWidgetDraw barWid $ \ctx -> do
    fill <- readTVarIO barFill
    True <$ C.renderWithContext (drawBar barWid fill) ctx
  pure Bar{..}
  where
    drawBar area fill = do
      barColor <- do
        ctxt <- Gtk.widgetGetStyleContext area
        Gtk.styleContextGetColor ctxt =<< Gtk.styleContextGetState ctxt
      red <- Gdk.getRGBARed barColor
      green <- Gdk.getRGBAGreen barColor
      blue <- Gdk.getRGBABlue barColor
      alpha <- Gdk.getRGBAAlpha barColor
      let (fillX, fillY) = case orient of
            Gtk.OrientationHorizontal -> (fill, 1.0)
            Gtk.OrientationVertical -> (1.0, fill)
            _ -> (1.0, 1.0)

      w <- Gtk.widgetGetAllocatedWidth area
      h <- Gtk.widgetGetAllocatedHeight area
      let RationalRect l t r d = relative
          left = realToFrac l * realToFrac w
          top = realToFrac t * realToFrac h
          right = realToFrac r * realToFrac w
          down = realToFrac d * realToFrac h
      C.setSourceRGBA red green blue alpha
      C.rectangle left down ((right - left) * fillX) ((top - down) * fillY)
      C.fill

barSetFill :: MonadIO m => Bar -> Double -> m ()
barSetFill Bar{..} fill = do
  liftIO . atomically $ writeTVar barFill fill
  Gtk.widgetQueueDraw barWid
