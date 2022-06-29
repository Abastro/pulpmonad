{-# LANGUAGE OverloadedLabels #-}

-- | Image-like Views.
module View.Imagery (
  ImageSet (..),
  ImageDyn,
  imageDynWidget,
  imageDynNew,
  imageDynSetImg,
  imageStaticNew,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.GI.Base.Constructible
import Data.Int
import Data.Text qualified as T
import GI.GdkPixbuf.Objects.Pixbuf qualified as Gdk
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gtk.Objects.Image qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Pixbufs qualified as Gtk

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
  imageDynImg <- new Gtk.Image []
  let imageDynSize = fromIntegral $ fromEnum size
  imageDynWid <- Gtk.toWidget imageDynImg
  when fixSize $ #setPixelSize imageDynImg $ Gtk.iconSizePx size
  pure ImageDyn{..}

imageDynSetImg :: MonadIO m => ImageDyn -> ImageSet -> m ()
imageDynSetImg ImageDyn{..} = \case
  ImgSName txt -> #setFromIconName imageDynImg (Just txt) imageDynSize
  ImgSGIcon icon -> #setFromGicon imageDynImg icon imageDynSize
  ImgSPixbuf pix -> #setFromPixbuf imageDynImg (Just pix)

-- | Static version of 'imageDynNew'.
imageStaticNew :: MonadIO m => Gtk.IconSize -> Bool -> ImageSet -> m Gtk.Widget
imageStaticNew size fixSize sets = do
  dyn <- imageDynNew size fixSize
  imageDynSetImg dyn sets
  pure $ imageDynWidget dyn
