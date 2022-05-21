-- | Status hook for X11 & EWMH.
-- X11: <https://x.org/releases/X11R7.6/doc/xorg-docs/specs/ICCCM/icccm.html>
-- EWMH: <https://specifications.freedesktop.org/wm-spec/wm-spec-1.3.html>.
module Status.WMStatus (
  DesktopStat (..),
  getDesktopStat,
  getWindowStat,
  WindowInfo (..),
  getWindowInfo,
  WMStateEx (..),
) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Bitraversable
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Foreign.Storable
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
import XMonad.Core
import XMonad.ManageHook (liftX)

-- TODO: https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.5.html
-- TODO: Need to subscribe on the X events

getWinProp :: Storable a => Int -> Atom -> MaybeT Query [a]
getWinProp size prop = do
  win <- ask
  MaybeT . liftX . withDisplay $ \display -> io $ rawGetWindowProperty size display prop win

getWinProp32 :: Storable a => Atom -> MaybeT Query [a]
getWinProp32 = getWinProp 32

getWinPropBytes :: Atom -> MaybeT Query BS.ByteString
getWinPropBytes prop = BS.pack <$> getWinProp 8 prop

onRoot :: Query a -> X a
onRoot query = asks theRoot >>= runQuery query

asSingle :: Applicative m => [a] -> MaybeT m a
asSingle = MaybeT . pure . listToMaybe

asUtf8 :: Monad m => BS.ByteString -> MaybeT m T.Text
asUtf8 = either (fail . show) pure . T.decodeUtf8'

asUtf8List :: Monad m => BS.ByteString -> MaybeT m [T.Text]
asUtf8List = traverse asUtf8 . filter (not . BS.null) . BS.split 0

-- | Encompassing Desktop status/statistics. Note, more desktops could be present than its names.
data DesktopStat = DesktopStat
  { numDesktops :: !Int
  , currentDesktop :: !Int
  , desktopNames :: !(V.Vector T.Text)
  , visibleDesktops :: !(V.Vector Int)
  -- ^ Visible desktops usually provided by XMonad. If not present, defaults to all named desktops.
  }

-- | Get the desktop statistics.
getDesktopStat :: X (Maybe DesktopStat)
getDesktopStat = do
  propNumDesk <- getAtom "_NET_NUMBER_OF_DESKTOPS"
  propCurDesk <- getAtom "_NET_CURRENT_DESKTOP"
  propDeskNames <- getAtom "_NET_DESKTOP_NAMES"
  propVisDesk <- getAtom "_XMONAD_VISIBLE_WORKSPACES"
  onRoot . runMaybeT $ do
    numDesktops <- asSingle =<< getWinProp32 @Int propNumDesk
    currentDesktop <- asSingle =<< getWinProp32 @Int propCurDesk
    guard $ currentDesktop >= 0 && currentDesktop < numDesktops

    -- Desktop names are optional
    desktopNames <- (<|> pure V.empty) $ do
      bytes <- getWinPropBytes propDeskNames
      V.fromList . take numDesktops <$> asUtf8List bytes

    -- Check if visible, this is also optional
    isVisible <- (<|> pure (const True)) $ do
      bytes <- getWinPropBytes propVisDesk
      flip S.member . S.fromList <$> asUtf8List bytes
    let visibleDesktops = V.findIndices isVisible desktopNames

    pure DesktopStat{..}

-- | Encompassing Window status/statistics.
data WindowStat = WindowStat
  { allWindows :: V.Vector Window
  , activeWindow :: Maybe Window
  }

getWindowStat :: X (Maybe WindowStat)
getWindowStat = do
  propClients <- getAtom "_NET_CLIENT_LIST"
  propActive <- getAtom "_NET_ACTIVE_WINDOW"
  onRoot . runMaybeT $ do
    allWindows <- V.fromList <$> getWinProp32 @Window propClients
    activeWindow <- do
      -- Only property check goes to error; empty list is considered as no active window.
      actives <- getWinProp32 @Window propActive
      pure (listToMaybe $ filter (> 0) actives)
    pure WindowStat{..}

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

-- | Window information. Only works for non-withdrawn state.
-- Still, the worst that could happen is giving Nothing.
getWindowInfo :: Query (Maybe WindowInfo)
getWindowInfo = do
  propWMDesk <- liftX $ getAtom "_NET_WM_DESKTOP"
  propNameExt <- liftX $ getAtom "_NET_WM_NAME"
  propName <- liftX $ getAtom "WM_NAME"
  propClass <- liftX $ getAtom "WM_CLASS"

  propWMSt <- liftX $ getAtom "_NET_WM_STATE"
  stMap <- liftX $ M.fromList <$> traverse (bitraverse getAtom pure) pairs
  runMaybeT $ do
    windowDesktop <- asSingle =<< getWinProp32 @Int propWMDesk
    windowTitle <- (asUtf8 =<< getWinPropBytes propNameExt) <|> (asUtf8 =<< getWinPropBytes propName)
    windowClasses <- V.fromList <$> (asUtf8List =<< getWinPropBytes propClass)
    windowState <- S.fromList . mapMaybe (stMap M.!?) <$> getWinProp32 @Atom propWMSt
    pure WindowInfo{..}
  where
    pairs =
      [ ("_NET_WM_STATE_HIDDEN", WinHidden)
      , ("_NET_WM_STATE_DEMANDS_ATTENTION", WinDemandAttention)
      ]
