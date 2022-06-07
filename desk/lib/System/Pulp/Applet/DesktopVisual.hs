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
  defShowFn,
) where

import Control.Exception.Enclosed
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Text qualified as T
import GI.GLib (castTo)
import GI.Gio.Interfaces.AppInfo
import GI.Gio.Objects.DesktopAppInfo
import Status.X11.WMStatus
import System.Pulp.Applet.DesktopVisual.Handle
import View.Imagery

-- TODO Somehow nautilus (and nautilus only) takes time to be notified

-- MAYBE: https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.5.html
-- TODO Optimize this one, maybe with watching it over & caching
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
      -- Some does not have StartupWmClass.
      -- So, check it against executable & desktop id with lowercased classes
      exec <- either (const Nothing) Just <$> tryAny (appInfoGetExecutable deskInfo)
      let exeName = exec >>= fmap T.pack . listToMaybe . words
      ident <- appInfoGetId deskInfo
      let isMatch cl =
            Just cl == wmClass
              || Just (T.toLower cl) == exeName
              || T.toLower cl <> T.pack ".desktop" == T.toLower ident
      pure (any isMatch classes)

defImageSetter :: WindowInfo -> MaybeT IO ImageSet
defImageSetter = const (fail "not handle")

defShowFn :: DesktopStat -> NumWindows -> Bool
defShowFn DesktopStat{desktopState} num = desktopState /= DeskHidden || num > 0
