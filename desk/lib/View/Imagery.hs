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
import Control.Monad.IO.Class
import Data.Int
import Data.Text qualified as T
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector qualified as C
import GI.Gdk.Structs.RGBA qualified as Gdk
import GI.GdkPixbuf.Objects.Pixbuf qualified as Gdk
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gtk.Flags qualified as UI
import GI.Gtk.Objects.Image qualified as UI
import UI.Commons qualified as UI
import UI.Styles qualified as UI
import XMonad.StackSet (RationalRect (..))

-- | Setting image for Image widget
data ImageSet = ImgSName T.Text | ImgSGIcon Gio.Icon | ImgSPixbuf Gdk.Pixbuf

data ImageDyn = ImageDyn
  { imageDynWid :: !UI.Widget
  , imageDynSize :: !Int32
  , imageDynImg :: !UI.Image
  }

imageDynWidget :: ImageDyn -> UI.Widget
imageDynWidget ImageDyn{imageDynWid} = imageDynWid

imageDynNew :: MonadIO m => UI.IconSize -> m ImageDyn
imageDynNew size = do
  imageDynImg <- UI.imageNew
  let imageDynSize = fromIntegral $ fromEnum size
  imageDynWid <- UI.toWidget imageDynImg
  pure ImageDyn{..}

imageDynSetImg :: MonadIO m => ImageDyn -> ImageSet -> m ()
imageDynSetImg ImageDyn{..} = \case
  ImgSName txt -> UI.imageSetFromIconName imageDynImg (Just txt) imageDynSize
  ImgSGIcon icon -> UI.imageSetFromGicon imageDynImg icon imageDynSize
  ImgSPixbuf pix -> UI.imageSetFromPixbuf imageDynImg (Just pix)

imageStaticNew :: MonadIO m => UI.IconSize -> ImageSet -> m UI.Widget
imageStaticNew size sets = do
  dyn <- imageDynNew size
  imageDynSetImg dyn sets
  pure $ imageDynWidget dyn

data BarColor = BarColor !Double !Double !Double

-- MAYBE Bar direction?
data Bar = Bar
  { barWid :: !UI.Widget
  , barFill :: !(TVar Double)
  }

barWidget :: Bar -> UI.Widget
barWidget Bar{barWid} = barWid

-- | Bar with initial fill of 0.
barNew :: MonadIO m => RationalRect -> m Bar
barNew relative = do
  barWid <- UI.toWidget =<< UI.imageNew -- Image is the most benign
  -- Cannot extend the class from haskell side, soo..
  UI.widgetGetStyleContext barWid >>= flip UI.styleContextAddClass (T.pack "bar")
  UI.setWidgetHalign barWid UI.AlignFill
  UI.setWidgetValign barWid UI.AlignFill
  barFill <- liftIO $ newTVarIO 0.0
  _ <- UI.onWidgetDraw barWid $ \ctx -> do
    fill <- readTVarIO barFill
    True <$ C.renderWithContext (drawBar barWid fill) ctx
  pure Bar{..}
  where
    drawBar area fill = do
      barColor <- (`UI.styleContextGetColor` [UI.StateFlagsNormal]) =<< UI.widgetGetStyleContext area
      red <- Gdk.getRGBARed barColor
      green <- Gdk.getRGBAGreen barColor
      blue <- Gdk.getRGBABlue barColor
      alpha <- Gdk.getRGBAAlpha barColor

      w <- UI.widgetGetAllocatedWidth area
      h <- UI.widgetGetAllocatedHeight area
      let RationalRect l t r d = relative
          left = realToFrac l * realToFrac w
          top = realToFrac t * realToFrac h
          right = realToFrac r * realToFrac w
          down = realToFrac d * realToFrac h
      C.setSourceRGBA red green blue alpha
      C.rectangle left down (right - left) ((top - down) * fill)
      C.fill

barSetFill :: MonadIO m => Bar -> Double -> m ()
barSetFill Bar{..} fill = do
  liftIO . atomically $ writeTVar barFill fill
  UI.widgetQueueDraw barWid
