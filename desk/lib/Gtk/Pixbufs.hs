module Gtk.Pixbufs (
  module GI.GdkPixbuf.Objects.Pixbuf
  , iconSizePx
  , RawIcon (..)
  , themeLoadIcon
  , iconsChoosePixbuf
  , argbTorgba
) where

import Control.Exception (try)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import Data.GI.Base.GError
import Data.Int
import Data.List
import Data.Text qualified as T
import GI.GLib.Structs.Bytes qualified as Glib
import GI.GdkPixbuf.Enums
import GI.GdkPixbuf.Objects.Pixbuf
import GI.Gtk.Enums (IconSize (..))
import GI.Gtk.Flags qualified as Gtk
import GI.Gtk.Objects.IconTheme qualified as Gtk

-- | Raw icon consisting of bytes representing RGBA/ARGB colors.
-- Left to right, Top to bottom.
data RawIcon = RawIcon
  { iconWidth :: !Int32
  , iconHeight :: !Int32
  , iconColors :: !BS.ByteString
  }

-- | Icon size as pixel size
iconSizePx :: IconSize -> Int32
iconSizePx = \case
  IconSizeMenu -> 16
  IconSizeSmallToolbar -> 16
  IconSizeLargeToolbar -> 24
  IconSizeButton -> 16
  IconSizeDnd -> 32
  IconSizeDialog -> 48
  _ -> 8

-- | Loads icon from a theme.
-- Returns Nothing on failure, not leaving error messages.
themeLoadIcon :: Gtk.IconTheme -> T.Text -> Int32 -> [Gtk.IconLookupFlags] -> IO (Maybe Pixbuf)
themeLoadIcon (theme :: Gtk.IconTheme) iconName size flags = do
  fromRight Nothing <$> try @GError (Gtk.iconThemeLoadIcon theme iconName size flags)

-- | Chooses icon into a pixbuf appropriate for given pixel size, with scaling if necessary.
--
-- Note that the icon is "assumed" to be square.
iconsChoosePixbuf :: MonadIO m => Int32 -> (BS.ByteString -> BS.ByteString) -> [RawIcon] -> m (Maybe Pixbuf)
iconsChoosePixbuf size converter icons = runMaybeT $ do
  RawIcon{..} : _ <- pure $ sortOn (\RawIcon{iconHeight} -> abs (iconHeight - size)) icons
  bytes <- Glib.bytesNew (Just $ converter iconColors)
  -- TODO How to GC the pixbuf?
  pixbuf <- pixbufNewFromBytes bytes ColorspaceRgb True 8 iconWidth iconHeight (iconWidth * nSample)
  -- Copies and scales here
  MaybeT $ pixbufScaleSimple pixbuf size size InterpTypeBilinear
  where
    nSample = 4

-- | Convert color format from ARGB to RGBA
argbTorgba :: BS.ByteString -> BS.ByteString
argbTorgba = LBS.toStrict . BS.toLazyByteString . convColor
  where
    convColor = \case
      emp | BS.null emp -> mempty
      colors
        | (argb, rem) <- BS.splitAt 4 colors ->
            BS.byteString (BS.tail argb) <> BS.word8 (BS.head argb) <> convColor rem
