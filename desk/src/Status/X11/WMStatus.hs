{-# LANGUAGE FunctionalDependencies #-}

-- | Status hook for X11 & EWMH.
-- X11: <https://x.org/releases/X11R7.6/doc/xorg-docs/specs/ICCCM/icccm.html>
-- EWMH: <https://specifications.freedesktop.org/wm-spec/wm-spec-1.3.html>.
module Status.X11.WMStatus (
  XProperty,
  XPropType (..),
  XPropGet,
  getProp,
  XPropQuery,
  readProp,
  watchXQueryWith,
  DesktopStat (..),
  getDesktopStat,
  WindowStats (..),
  getWindowStats,
  WMStateEx (..),
  WindowInfo (..),
  getWindowInfo,
) where

import Control.Applicative
import Control.Concurrent.Task
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Bitraversable
import Data.Bits
import Data.ByteString qualified as BS
import Data.IORef
import Data.Int
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Data.Word
import Foreign.Storable
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
import Status.X11.XHandle

-- MAYBE: https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.5.html
-- TODO: Need to subscribe on the X events

asUtf8 :: MonadFail m => BS.ByteString -> m T.Text
asUtf8 = either (fail . show) pure . T.decodeUtf8'

-- | As Utf8 list separated by '\0'.
asUtf8List :: MonadFail m => BS.ByteString -> m [T.Text]
asUtf8List = traverse asUtf8 . filter (not . BS.null) . BS.split 0

-- | Denotes a property, with type attached with parsing capability.
data XProperty a = XProperty Atom

-- | Property type that can be parsed from native.
class Storable n => XPropType n a | a -> n where
  parseProp :: [n] -> Maybe a

instance XPropType Int32 Int where parseProp = fmap fromIntegral . listToMaybe
instance XPropType Word32 XID where parseProp = fmap fromIntegral . listToMaybe

instance XPropType Word32 [XID] where parseProp = Just . fmap fromIntegral

instance XPropType Word8 BS.ByteString where parseProp = Just . BS.pack
instance XPropType Word8 T.Text where parseProp = parseProp >=> asUtf8
instance XPropType Word8 [T.Text] where parseProp = parseProp >=> asUtf8List

-- | Applicative for getting the required property atoms, and perhaps others.
-- Wraps X11 since Resolving `Atom` requires X call.
-- Only implements Applicative since failure-capable monadic should not happen.
newtype XPropGet a = XPropGet (X11 (IORef [Atom]) a)
  deriving (Functor, Applicative, ActX11)

getProp :: forall a. String -> XPropGet (XProperty a)
getProp name = XPropGet $ do
  X11Env{extData = tracked} <- ask
  property <- xAtom name
  liftIO $ modifyIORef' tracked (property :)
  pure (XProperty property)

-- | X Property query monad, which is simply MaybeT around X event handler.
-- Make sure that no exception leaks out.
type XPropQuery = MaybeT XEventHandle
-- ^ TODO MaybeT is suboptimal.

readProp :: forall a n. XPropType n a => XProperty a -> XPropQuery a
readProp (XProperty prop) = do
  disp <- xDisplay
  win <- xWindow
  let size = 8 * sizeOf @n undefined
  raw <- MaybeT . liftIO $ rawGetWindowProperty @n size disp prop win
  MaybeT . pure $ parseProp raw

data XQuerySpec a = forall p.
  XQuerySpec
  { getProps :: XPropGet p
  , queryProps :: p -> XPropQuery a
  }

-- | X query that watches for X property at certain window.
-- Any query which fail to produce result doesn't emit the task.
-- Can also listen to other events, while it cannot emit the task.
watchXQueryWith :: XQuerySpec a -> EventMask -> (Event -> XEventHandle ()) -> XEventHandle (Task a)
watchXQueryWith XQuerySpec{getProps = XPropGet getPrs, queryProps} otherMask otherEv = do
  window <- xWindow
  watched <- liftIO (newIORef [])
  props <- x11ToEventHandle watched getPrs
  watchSet <- S.fromList <$> liftIO (readIORef watched)
  -- TODO Eh, silently not emit anything? Really?
  initial <- runMaybeT (queryProps props)
  xListenTo (propertyChangeMask .|. otherMask) window initial $ \case
    PropertyEvent{ev_atom = prop}
      | prop `S.member` watchSet -> runMaybeT (queryProps props)
    event -> Nothing <$ otherEv event

data DesktopState = DeskActive | DeskVisible | DeskHidden

-- | Desktop status for each desktop.
data DesktopStat = DesktopStat
  { desktopName :: Maybe T.Text
  -- ^ Name of the desktop. May not exist.
  , desktopState :: !DesktopState
  }

data DeskStatProps = DeskStatProps
  { propNumDesk :: XProperty Int
  , propCurDesk :: XProperty Int
  , propDeskNames :: XProperty [T.Text]
  , propVisDesk :: XProperty [T.Text]
  }

-- | Get the vector of desktop status.
getDesktopStat :: XQuerySpec (V.Vector DesktopStat)
getDesktopStat =
  XQuerySpec
    { getProps =
        DeskStatProps
          <$> getProp "_NET_NUMBER_OF_DESKTOPS"
          <*> getProp "_NET_CURRENT_DESKTOP"
          <*> getProp "_NET_DESKTOP_NAMES"
          <*> getProp "_XMONAD_VISIBLE_WORKSPACES"
    , queryProps = \DeskStatProps{..} -> do
        numDesktops <- readProp propNumDesk
        currentDesktop <- readProp propCurDesk
        guard $ currentDesktop >= 0 && currentDesktop < numDesktops

        desktopNames <- (<|> pure V.empty) $ do
          V.fromList . take numDesktops <$> readProp propDeskNames
        isVisible <- (<|> pure (const True)) $ do
          flip S.member . S.fromList <$> readProp propVisDesk

        let deskInfo idx = (DesktopStat <*> deskState idx) (desktopNames V.!? idx)
            deskState idx = \case
              _ | idx == currentDesktop -> DeskActive
              Just name | isVisible name -> DeskVisible
              _ -> DeskHidden
        pure $ V.generate numDesktops deskInfo
    }

-- | Encompassing Window statistics.
data WindowStats = WindowStats
  { allWindows :: V.Vector Window
  , activeWindow :: Maybe Window
  }

data WindowStatsProps = WindowStatsProps
  {propClients :: XProperty [Window], propActive :: XProperty [Window]}

-- | Get encompassing window statistics.
getWindowStats :: XQuerySpec WindowStats
getWindowStats =
  XQuerySpec
    { getProps = WindowStatsProps <$> getProp "_NET_CLIENT_LIST" <*> getProp "_NET_ACTIVE_WINDOW"
    , queryProps = \WindowStatsProps{..} -> do
        allWindows <- V.fromList <$> readProp propClients
        -- Empty list is considered as no active window.
        activeWindow <- listToMaybe . filter (> 0) <$> readProp propActive
        pure WindowStats{..}
    }

-- MAYBE How to deal with WM_ICON? It's quite hard.

data WMStateEx = WinHidden | WinDemandAttention
  deriving (Eq, Ord)

-- | Information on the specific window.
data WindowInfo = WindowInfo
  { windowDesktop :: !Int
  -- ^ Desktop ID a window resides on.
  , windowTitle :: !T.Text
  , windowClasses :: !(V.Vector T.Text)
  , windowState :: !(S.Set WMStateEx)
  }

data WindowInfoProps = WindowInfoProps
  { propWMDesk :: XProperty Int
  , propNameExt :: XProperty T.Text
  , propName :: XProperty T.Text
  , propClass :: XProperty [T.Text]
  , propWMState :: XProperty [Atom]
  , propSTMap :: M.Map Atom WMStateEx
  }

-- | Window information. Only works for non-withdrawn state.
-- Still, the worst that could happen is giving Nothing.
getWindowInfo :: Window -> XQuerySpec WindowInfo
getWindowInfo _window =
  XQuerySpec
    { getProps =
        WindowInfoProps
          <$> getProp "_NET_WM_DESKTOP"
          <*> getProp "_NET_WM_NAME"
          <*> getProp "WM_NAME"
          <*> getProp "WM_CLASS"
          <*> getProp "_NET_WM_STATE"
          <*> (M.fromList <$> traverse (bitraverse xAtom pure) pairs)
    , queryProps = \WindowInfoProps{..} -> do
        undefined $ do
          -- MAYBE bound check? May not make sense here.
          windowDesktop <- readProp propWMDesk
          windowTitle <- readProp propNameExt <|> readProp propName
          windowClasses <- V.fromList <$> readProp propClass
          windowState <- S.fromList . mapMaybe (propSTMap M.!?) <$> readProp propWMState
          pure WindowInfo{..}
    }
  where
    pairs =
      [ ("_NET_WM_STATE_HIDDEN", WinHidden)
      , ("_NET_WM_STATE_DEMANDS_ATTENTION", WinDemandAttention)
      ]
