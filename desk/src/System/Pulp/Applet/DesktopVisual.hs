{-# LANGUAGE MonoLocalBinds #-}

-- | Desktops visualizer widget.
module System.Pulp.Applet.DesktopVisual (
  ImageSet (..),
  NumWindows,
  DesktopSetup (..),
  WindowSetup (..),
  deskVisualizer,
  defImageSetter,
  appInfoImageSetter,
  classImageSetter,
  defShowFn,
) where

import Control.Applicative
import Control.Exception.Enclosed
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BS
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text qualified as T
import GI.GLib (castTo)
import GI.GLib.Structs.Bytes qualified as Glib
import GI.GdkPixbuf.Enums qualified as Gdk
import GI.GdkPixbuf.Objects.Pixbuf qualified as Gdk
import GI.Gio.Interfaces.AppInfo
import GI.Gio.Objects.DesktopAppInfo
import GI.Gtk.Objects.IconTheme qualified as UI
import Status.X11.WMStatus
import System.Log.Logger
import System.Pulp.Applet.DesktopVisual.Handle
import System.Pulp.Applet.DesktopVisual.View (ImageSet (..))
import qualified Data.ByteString.Lazy as LBS

-- MAYBE: https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.5.html
-- TODO Optimize this one, maybe with caching
appInfoImageSetter :: WindowInfo -> MaybeT IO ImageSet
appInfoImageSetter WindowInfo{..} = do
  icon <- getAlt $ foldMap Alt $ findIcon <$> windowClasses
  pure (ImgSGIcon icon)
  where
    findIcon className = MaybeT $ do
      allInfos <- appInfoGetAll
      deskInfos <- catMaybes <$> traverse (castTo DesktopAppInfo) allInfos
      filtered <- filterM (fmap (== Just className) . appWmClass) deskInfos
      join . listToMaybe <$> traverse appInfoGetIcon filtered
    appWmClass appInfo =
      either (const Nothing) Just
        <$> tryAny (desktopAppInfoGetStartupWmClass appInfo)

classImageSetter :: WindowInfo -> MaybeT IO ImageSet
classImageSetter WindowInfo{windowClasses} = do
  iconTheme <- UI.iconThemeGetDefault
  iconName <- getAlt $ foldMap Alt $ findIcon iconTheme <$> windowClasses
  pure (ImgSName iconName)
  where
    findIcon iconTheme className = do
      UI.iconThemeHasIcon iconTheme className >>= guard
      pure className

-- TODO Properly adapt the icon size
xIconImageSetter :: GetXIcon -> MaybeT IO ImageSet
xIconImageSetter getXIcon =
  liftIO getXIcon >>= \case
    Left err -> MaybeT $ Nothing <$ liftIO (infoM "DeskVis" $ "Cannot recognize icon: " <> err)
    Right icons -> do
      -- If empty, fails on MaybeT, i.e. give Nothing.
      XIcon{..} : _ <- pure $ sortOn (\XIcon{iconHeight} -> abs (iconHeight - 24)) icons
      -- Convert: ARGB -> RGBA
      let converted = LBS.toStrict . BS.toLazyByteString $ convColor iconColors
      bytes <- Glib.bytesNew (Just converted)
      -- TODO How to GC the pixbuf?
      let width = fromIntegral iconWidth
      let height = fromIntegral iconHeight
      pixbuf <- Gdk.pixbufNewFromBytes bytes Gdk.ColorspaceRgb True 8 width height (width * 4)
      pure $ ImgSPixbuf pixbuf
  where
    convColor emp | True <- BS.null emp = mempty
    convColor colors
      | (argb, rem) <- BS.splitAt 4 colors =
          BS.byteString (BS.tail argb) <> BS.word8 (BS.head argb) <> convColor rem

defImageSetter :: WindowInfo -> GetXIcon -> IO ImageSet
defImageSetter winInfo getXIcon = do
  imageSet <-
    runMaybeT $
      appInfoImageSetter winInfo
        <|> classImageSetter winInfo
        <|> xIconImageSetter getXIcon
  pure (fromMaybe (ImgSName $ T.pack "missing") imageSet)

defShowFn :: DesktopStat -> NumWindows -> Bool
defShowFn DesktopStat{desktopState} num = desktopState /= DeskHidden || num > 0
