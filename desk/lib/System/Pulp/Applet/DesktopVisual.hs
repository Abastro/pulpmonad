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
import Data.Maybe
import Data.Monoid
import Data.Text qualified as T
import GI.GLib (castTo)
import GI.Gio.Interfaces.AppInfo
import GI.Gio.Objects.DesktopAppInfo
import GI.Gtk.Objects.IconTheme qualified as UI
import Status.X11.WMStatus
import System.Pulp.Applet.DesktopVisual.Handle
import View.Imagery
import qualified UI.Pixbufs as UI

-- MAYBE: https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.5.html
-- TODO Optimize this one, maybe with caching
-- TODO Gnome apps (e.g. terminal) is harder to detect. How to go around?
appInfoImageSetter :: WindowInfo -> MaybeT IO ImageSet
appInfoImageSetter WindowInfo{..} = do
  icon <- findIcon windowClasses
  pure (ImgSGIcon icon)
  where
    findIcon classes = MaybeT $ do
      allInfos <- appInfoGetAll
      deskInfos <- catMaybes <$> traverse (castTo DesktopAppInfo) allInfos
      filtered <- filterM (filterCond classes) deskInfos
      join . listToMaybe <$> traverse appInfoGetIcon filtered

    filterCond classes deskInfo = do
      wmClass <- either (const Nothing) Just <$> tryAny (desktopAppInfoGetStartupWmClass deskInfo)
      -- Some slips through, so check it against desktop id with lowercased classes
      ident <- appInfoGetId deskInfo
      let isMatch cl = Just cl == wmClass || T.toLower cl <> T.pack ".desktop" == ident
      pure (any isMatch classes)

classImageSetter :: WindowInfo -> MaybeT IO ImageSet
classImageSetter WindowInfo{windowClasses} = do
  iconTheme <- UI.iconThemeGetDefault
  iconName <- getAlt $ foldMap Alt $ findIcon iconTheme <$> windowClasses
  pure (ImgSName iconName)
  where
    findIcon iconTheme className = do
      UI.iconThemeHasIcon iconTheme className >>= guard
      pure className

-- MAYBE Proper logging
xIconImageSetter :: WindowInfo -> GetXIcon -> MaybeT IO ImageSet
xIconImageSetter WindowInfo{windowClasses} getXIcon =
  liftIO getXIcon >>= \case
    Left err -> MaybeT $ Nothing <$ liftIO (putStrLn $ "Cannot recognize icon: " <> err)
    Right icons -> do
      liftIO $ putStrLn $ "X11 icon getting for " <> show windowClasses
      scaled <- MaybeT $ UI.iconsChoosePixbuf 24 UI.argbTorgba icons
      pure (ImgSPixbuf scaled)

defImageSetter :: WindowInfo -> GetXIcon -> IO ImageSet
defImageSetter winInfo getXIcon = do
  imageSet <-
    runMaybeT $
      appInfoImageSetter winInfo
        <|> classImageSetter winInfo
        <|> xIconImageSetter winInfo getXIcon
  pure (fromMaybe (ImgSName $ T.pack "missing") imageSet)

defShowFn :: DesktopStat -> NumWindows -> Bool
defShowFn DesktopStat{desktopState} num = desktopState /= DeskHidden || num > 0
