-- | Status hook for X11 & EWMH.
-- X11: <https://x.org/releases/X11R7.6/doc/xorg-docs/specs/ICCCM/icccm.html>
-- EWMH: <https://specifications.freedesktop.org/wm-spec/wm-spec-1.3.html>.
module Status.WMStatus (
  XQEnv,
  XQuery,
  evalXQuery,
  DesktopStat (..),
  getDesktopStat,
  getWindowStats,
  WindowInfo (..),
  getWindowInfo,
  WMStateEx (..),
) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Task
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Bitraversable
import Data.ByteString qualified as BS
import Data.Foldable
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Traversable
import Data.Vector qualified as V
import Foreign.Storable
import Graphics.X11.Types
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Status.XHandle

-- MAYBE: https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.5.html
-- TODO: Need to subscribe on the X events

-- | X Query environment. Not built for thread-safety, do not share it between threads.
newtype XQEnv = XQEnv (IORef [Atom])

-- | Denotes a property ID, so that it could be listened upon.
newtype XPropID = XPropID Atom
  deriving (Eq, Ord)

-- | Applicative for getting the required property atoms, and perhaps others.
-- Wraps X11 since Resolving `Atom` requires X call.
-- Only implements Applicative since arbitrary IO action should not happen.
newtype XProps a = XProps (X11 (IORef [XPropID]) a)
  deriving (Functor, Applicative)

getPropID :: String -> XProps XPropID
getPropID name = XProps $ do
  X11Env{extData = tracked} <- ask
  property <- XPropID <$> xAtom name
  liftIO $ modifyIORef' tracked (property :)
  pure property

-- | Gets non-property atom.
getXAtom :: String -> XProps Atom
getXAtom name = XProps (xAtom name)

data XQuer a = forall p. XQuer
  { getProps :: XProps p
  , queryProps :: p -> MaybeT (X11 ()) a
  }

-- | Watch for X property event for changes, including one from child windows.
--
-- NB: When query could not be performed, no event is received.
-- TODO From child windows?
watchXQuery :: XQuer a -> X11 () (Maybe (Task a))
watchXQuery XQuer{ getProps = (XProps getPrs), queryProps} = do
  watched <- liftIO $ newIORef []
  propBase <- withXExt (\() -> watched) getPrs
  watchSet <- S.fromList <$> liftIO (readIORef watched)
  init <- runMaybeT (queryProps propBase)
  for init $ \initial -> do
    startWithRepeater xEventLoop initial $ \case
      PropertyEvent{ev_atom = prop} | XPropID prop `S.member` watchSet -> do
        runMaybeT (queryProps propBase)
      _ -> pure Nothing
    undefined
  undefined

-- Hmm, we should always track the property atoms - instead of guarding it behind.

-- MAYBE MaybeT is suboptimal. Proper errors?

-- | X11 Query monad. Should NOT call exceptional code in this section.
type XQuery = MaybeT (X11 XQEnv)

runXQuery :: XQuery a -> X11 () (Maybe (a, [Atom]))
runXQuery query = do
  watched <- liftIO $ newIORef []
  withXExt (\() -> XQEnv watched) (runMaybeT query) >>= \case
    Nothing -> pure Nothing
    Just val -> Just . (val,) <$> liftIO (readIORef watched)

-- | Simply evaluate an X query.
evalXQuery :: XQuery a -> X11 () (Maybe a)
evalXQuery query = fmap fst <$> runXQuery query

queryWinProp :: Storable a => Int -> Atom -> XQuery [a]
queryWinProp size prop = do
  X11Env{extData = XQEnv tracked, ..} <- ask
  liftIO $ modifyIORef' tracked (prop :)
  MaybeT . liftIO $ rawGetWindowProperty size theDisplay prop targetWindow

queryWinProp32 :: Storable a => Atom -> XQuery [a]
queryWinProp32 = queryWinProp 32

queryWinPropBytes :: Atom -> XQuery BS.ByteString
queryWinPropBytes prop = BS.pack <$> queryWinProp 8 prop

asSingle :: Applicative m => [a] -> MaybeT m a
asSingle = MaybeT . pure . listToMaybe

asUtf8 :: Monad m => BS.ByteString -> MaybeT m T.Text
asUtf8 = either (fail . show) pure . T.decodeUtf8'

asUtf8List :: Monad m => BS.ByteString -> MaybeT m [T.Text]
asUtf8List = traverse asUtf8 . filter (not . BS.null) . BS.split 0

data DesktopState = DeskActive | DeskVisible | DeskHidden

-- | Desktop status for each desktop.
data DesktopStat = DesktopStat
  { desktopName :: Maybe T.Text
  -- ^ Name of the desktop. May not exist.
  , desktopState :: !DesktopState
  }

-- | Get the vector of desktop status.
getDesktopStat :: XQuery (V.Vector DesktopStat)
getDesktopStat = do
  propNumDesk <- lift $ xAtom "_NET_NUMBER_OF_DESKTOPS"
  propCurDesk <- lift $ xAtom "_NET_CURRENT_DESKTOP"
  propDeskNames <- lift $ xAtom "_NET_DESKTOP_NAMES"
  propVisDesk <- lift $ xAtom "_XMONAD_VISIBLE_WORKSPACES"

  numDesktops <- asSingle =<< queryWinProp32 @Int propNumDesk
  currentDesktop <- asSingle =<< queryWinProp32 @Int propCurDesk
  guard $ currentDesktop >= 0 && currentDesktop < numDesktops

  -- Desktop names are optional
  desktopNames <- (<|> pure V.empty) $ do
    bytes <- queryWinPropBytes propDeskNames
    V.fromList . take numDesktops <$> asUtf8List bytes

  -- Check if visible, this is also optional
  isVisible <- (<|> pure (const True)) $ do
    bytes <- queryWinPropBytes propVisDesk
    flip S.member . S.fromList <$> asUtf8List bytes

  let deskInfo idx = (DesktopStat <*> deskState idx) (desktopNames V.!? idx)
      deskState idx = \case
        _ | idx == currentDesktop -> DeskActive
        Just name | isVisible name -> DeskVisible
        _ -> DeskHidden
  pure $ V.generate numDesktops deskInfo

-- | Encompassing Window statistics.
data WindowStats = WindowStats
  { allWindows :: V.Vector Window
  , activeWindow :: Maybe Window
  }

-- | Get encompassing window statistics.
getWindowStats :: XQuery WindowStats
getWindowStats = do
  propClients <- lift $ xAtom "_NET_CLIENT_LIST"
  propActive <- lift $ xAtom "_NET_ACTIVE_WINDOW"

  allWindows <- V.fromList <$> queryWinProp32 @Window propClients
  activeWindow <- do
    -- Only property check goes to error; empty list is considered as no active window.
    actives <- queryWinProp32 @Window propActive
    pure (listToMaybe $ filter (> 0) actives)
  pure WindowStats{..}

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
getWindowInfo :: Window -> XQuery WindowInfo
getWindowInfo window = do
  propWMDesk <- lift $ xAtom "_NET_WM_DESKTOP"
  propNameExt <- lift $ xAtom "_NET_WM_NAME"
  propName <- lift $ xAtom "WM_NAME"
  propClass <- lift $ xAtom "WM_CLASS"

  propWMSt <- lift $ xAtom "_NET_WM_STATE"
  stMap <- M.fromList <$> traverse (bitraverse (lift . xAtom) pure) pairs

  mapMaybeT (onXWindow window) $ do
    -- MAYBE bound check? May not make sense here.
    windowDesktop <- asSingle =<< queryWinProp32 @Int propWMDesk
    windowTitle <- (asUtf8 =<< queryWinPropBytes propNameExt) <|> (asUtf8 =<< queryWinPropBytes propName)
    windowClasses <- V.fromList <$> (asUtf8List =<< queryWinPropBytes propClass)
    windowState <- S.fromList . mapMaybe (stMap M.!?) <$> queryWinProp32 @Atom propWMSt
    pure WindowInfo{..}
  where
    pairs =
      [ ("_NET_WM_STATE_HIDDEN", WinHidden)
      , ("_NET_WM_STATE_DEMANDS_ATTENTION", WinDemandAttention)
      ]
