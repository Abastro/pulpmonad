{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Status hook for X11 & EWMH.
-- X11: <https://x.org/releases/X11R7.6/doc/xorg-docs/specs/ICCCM/icccm.html>
-- EWMH: <https://specifications.freedesktop.org/wm-spec/wm-spec-1.3.html>.
module Status.X11.WMStatus (
  XPropType (..),
  XQueryError (..),
  formatXQError,
  XPQuery,
  queryProp,
  prepareForQuery,
  runXQuery,
  watchXQuery,
  DesktopState (..),
  DesktopStat (..),
  getDesktopStat,
  getAllWindows,
  getActiveWindow,
  WMStateEx (..),
  WindowInfo (..),
  getWindowInfo,
) where

import Control.Applicative
import Control.Concurrent.Task
import Control.Monad.Except
import Data.Bitraversable
import Data.ByteString qualified as BS
import Data.Functor.Compose
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Proxy
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Validation
import Data.Vector qualified as V
import Foreign.C.Types
import Foreign.Storable
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
import Status.X11.XHandle
import Text.Printf

-- MAYBE: https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.5.html
-- TODO: Need to subscribe on the X events

asUtf8 :: MonadError T.Text m => BS.ByteString -> m T.Text
asUtf8 = either (throwError . T.pack . show) pure . T.decodeUtf8'

-- | As Utf8 list separated by '\0'.
asUtf8List :: MonadError T.Text m => BS.ByteString -> m [T.Text]
asUtf8List = traverse asUtf8 . filter (not . BS.null) . BS.split 0

asSingle :: MonadError T.Text m => [a] -> m a
asSingle = \case
  [] -> throwError $ T.pack "empty list"
  x : _ -> pure x

-- | Since the format X emits is somewhat intricate.
class Storable n => XPropRaw n where bitSize :: proxy n -> Int

instance XPropRaw CChar where bitSize _ = 8
instance XPropRaw CShort where bitSize _ = 16
instance XPropRaw CLong where bitSize _ = 32 -- Yeah, I don't get why even is this..

-- | Property type that can be parsed from native.
class XPropRaw n => XPropType n a | a -> n where
  parseProp :: [n] -> Either T.Text a

instance XPropType CLong Int where parseProp = fmap fromIntegral . asSingle
instance XPropType CLong XID where parseProp = fmap fromIntegral . asSingle

instance XPropType CLong [XID] where parseProp = Right . fmap fromIntegral

instance XPropType CChar BS.ByteString where parseProp = Right . BS.pack . fmap fromIntegral
instance XPropType CChar T.Text where parseProp = parseProp >=> asUtf8
instance XPropType CChar [T.Text] where parseProp = parseProp >=> asUtf8List

-- | Query error type.
data XQueryError = XMissingProperty String | XParseError String T.Text
  deriving (Show)

-- | Format list of query errors with window. WIP.
formatXQError :: Window -> [XQueryError] -> String
formatXQError window errors = printf "Query error[%d]: %s" window (show errors)

-- TODO Make the reference to property atoms optional

-- | X Property query applicative, which accumulates all the errors.
-- Make sure that no exception leaks out.
newtype XPQuery a = XPQuery (XIO (IORef [Atom]) (Validation [XQueryError] a))
  deriving (Functor, Applicative, ActX11) via (Compose (XIO (IORef [Atom])) (Validation [XQueryError]))

instance Alternative XPQuery where
  empty = XPQuery (pure empty)
  XPQuery qa <|> XPQuery qb = XPQuery (liftA2 (<|>) qa qb)

queryProp :: forall a n. (XPropType n a) => String -> XPQuery a
queryProp name = XPQuery $ do
  tracked <- xGetExt
  prop <- xAtom name
  liftIO $ modifyIORef' tracked (prop :)
  let bits = bitSize $ Proxy @n
  mayRaw <- liftDWIO $ \disp win -> rawGetWindowProperty @n bits disp prop win
  pure (either (Failure . (: [])) Success $ handleParse mayRaw)
  where
    handleParse mayRaw = do
      raw <- maybe (throwError $ XMissingProperty name) pure mayRaw
      either (throwError . XParseError name) pure (parseProp @n @a raw)

-- | A bit of a hack, interns some atoms for the query action.
{-# WARNING prepareForQuery "Hack for interning more atoms. Could be removed" #-}
prepareForQuery :: XIO () a -> (a -> XPQuery b) -> XPQuery b
prepareForQuery prepare getQuery = XPQuery $ do
  prepared <- xWithExt (\_ -> ()) prepare
  let XPQuery query = getQuery prepared in query

-- INTERNAL for query run
runXInt :: XPQuery a -> XIO () (Either [XQueryError] a, S.Set Atom)
runXInt (XPQuery query) = do
  watched <- liftIO (newIORef [])
  val <- xWithExt (\() -> watched) query
  watchSet <- S.fromList <$> liftIO (readIORef watched)
  pure (validToEither val, watchSet)

-- | Simply runs X query once.
runXQuery :: XPQuery a -> XIO () (Either [XQueryError] a)
runXQuery (XPQuery query) = do
  placeholder <- liftIO (newIORef [])
  validToEither <$> xWithExt (\() -> placeholder) query

-- | Watch X query at the given window.
-- Note, allows some modifier process in between.
-- Immediately runs query to get current property right away.
--
-- Only the first query deliver error message,
-- and any other query which fail to produce result doesn't emit the task.
watchXQuery ::
  Window ->
  XPQuery a ->
  (a -> ExceptT [XQueryError] (XIO ()) b) ->
  XIO () (Either [XQueryError] (Task b))
watchXQuery window query modifier = do
  (inited, watchSet) <- xOnWindow window $ runXInt query
  applyModify inited >>= traverse \initial -> do
    xListenTo propertyChangeMask window (Just initial) $ \case
      PropertyEvent{ev_atom = prop}
        | prop `S.member` watchSet -> failing <$> (runXQuery query >>= applyModify)
      _ -> pure Nothing
  where
    failing = either (fail . show) pure
    applyModify pre = runExceptT (either throwError modifier pre)

data DesktopState
  = -- | Active desktop
    DeskActive
  | -- | Visible desktop from other screens in Xinerama
    DeskVisible
  | -- | Other hidden desktops
    DeskHidden
  deriving (Eq, Ord, Enum, Bounded)

-- | Desktop status for each desktop.
data DesktopStat = DesktopStat
  { desktopName :: Maybe T.Text
  -- ^ Name of the desktop. May not exist.
  , desktopState :: !DesktopState
  }

-- | Get the vector of desktop status.
getDesktopStat :: XPQuery (V.Vector DesktopStat)
getDesktopStat =
  asStat
    <$> numDesktops
    <*> curDesktop
    <*> (allDeskNames <|> pure V.empty)
    <*> (flip S.member <$> allVisibles <|> pure (const True))
  where
    -- MAYBE Bound check? Admittedly, that is quite unnecessary
    numDesktops = queryProp @Int "_NET_NUMBER_OF_DESKTOPS"
    curDesktop = queryProp @Int "_NET_CURRENT_DESKTOP"
    allDeskNames = V.fromList <$> queryProp @[T.Text] "_NET_DESKTOP_NAMES"
    allVisibles = S.fromList <$> queryProp @[T.Text] "_XMONAD_VISIBLE_WORKSPACES"
    asStat numDesk curDesk allNames isVisible = V.generate numDesk deskInfo
      where
        deskInfo idx = (DesktopStat <*> deskState idx) (allNames V.!? idx)
        deskState idx = \case
          _ | idx == curDesk -> DeskActive
          Just name | isVisible name -> DeskVisible
          _ -> DeskHidden

-- | Get all windows.
getAllWindows :: XPQuery (S.Set Window)
getAllWindows = S.fromList <$> queryProp @[Window] "_NET_CLIENT_LIST"

-- | Get an active window.
getActiveWindow :: XPQuery (Maybe Window)
getActiveWindow = listToMaybe . filter (> 0) <$> queryProp @[Window] "_NET_ACTIVE_WINDOW"

-- MAYBE How to deal with WM_ICON? It's quite hard.

-- | Inclusive states of the window.
data WMStateEx = WinHidden | WinDemandAttention
  deriving (Eq, Ord, Enum, Bounded)

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
getWindowInfo :: XPQuery WindowInfo
getWindowInfo = WindowInfo <$> wmDesktop <*> wmTitle <*> wmClass <*> wmState
  where
    -- Missing location on desktop: -1
    wmDesktop = queryProp @Int "_NET_WM_DESKTOP" <|> pure (-1)
    -- Let's tolerate missing title.
    wmTitle = (queryProp @T.Text "_NET_WM_NAME" <|> queryProp @T.Text "WM_NAME") <|> pure T.empty
    wmClass = V.fromList <$> queryProp @[T.Text] "WM_CLASS"
    wmState = prepareForQuery stateAtoms $ \stMap -> S.fromList . mapMaybe (stMap M.!?) <$> wmStateRaw
    wmStateRaw = queryProp @[Atom] "_NET_WM_STATE" <|> pure [] -- State is optional
    stateAtoms = M.fromList <$> traverse (bitraverse xAtom pure) pairs
    pairs =
      [ ("_NET_WM_STATE_HIDDEN", WinHidden)
      , ("_NET_WM_STATE_DEMANDS_ATTENTION", WinDemandAttention)
      ]
