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
  reqCurrentDesktop,
  getDesktopLayout,
  LayoutCmd (..),
  reqDesktopLayout,
  getAllWindows,
  getActiveWindow,
  reqActiveWindow,
  WMStateEx (..),
  WindowInfo (..),
  getWindowInfo,
  getWindowDesktop,
  getWindowIcon,
) where

import Control.Applicative
import Control.Concurrent.Task
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bitraversable
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Compose
import Data.IORef
import Data.List
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
import Gtk.Pixbufs qualified as Gtk
import Status.X11.XHandle
import Text.Printf

asUtf8 :: MonadError T.Text m => BS.ByteString -> m T.Text
asUtf8 = either (throwError . T.pack . show) pure . T.decodeUtf8'

-- | As Utf8 list separated by '\0'.
asUtf8List :: MonadError T.Text m => BS.ByteString -> m [T.Text]
asUtf8List = traverse asUtf8 . filter (not . BS.null) . BS.split 0

asSingle :: MonadError T.Text m => [a] -> m a
asSingle = \case
  [] -> throwError $ T.pack "empty list"
  x : _ -> pure x

-- | The format X emits is somewhat intricate
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

-- | X Property query applicative, which accumulates all the errors.
-- Make sure that no exception leaks out.
newtype XPQuery a = XPQuery (ReaderT (IORef [Atom]) XIO (Validation [XQueryError] a))
  deriving
    (Functor, Applicative, ActX11)
    via (ReaderT (IORef [Atom]) (Compose XIO (Validation [XQueryError])))

instance Alternative XPQuery where
  empty = XPQuery (pure empty)
  XPQuery qa <|> XPQuery qb = XPQuery (liftA2 (<|>) qa qb)

-- TODO `rawGetWindowProperty` does not stream. Check: can we afford allocating whole list for this?
queryProp :: forall a n. (XPropType n a) => String -> XPQuery a
queryProp name = XPQuery $ do
  tracked <- ask
  prop <- xAtom name
  liftIO $ modifyIORef' tracked (prop :)
  let bits = bitSize $ Proxy @n
  mayRaw <- liftDWIO $ \disp win -> rawGetWindowProperty @n bits disp prop win
  pure $ either (Failure . (: [])) Success $ handleParse mayRaw
  where
    handleParse mayRaw = do
      raw <- maybe (throwError $ XMissingProperty name) pure mayRaw
      either (throwError . XParseError name) pure (parseProp @n @a raw)

-- | A bit of a hack, interns some atoms for the query action.
{-# WARNING prepareForQuery "Hack for interning more atoms. Could be removed" #-}
prepareForQuery :: XIO a -> (a -> XPQuery b) -> XPQuery b
prepareForQuery prepare getQuery = XPQuery $ do
  prepared <- lift prepare
  let XPQuery query = getQuery prepared in query

-- INTERNAL for query run
runXInt :: XPQuery a -> XIO (Either [XQueryError] a, S.Set Atom)
runXInt (XPQuery query) = do
  watched <- liftIO (newIORef [])
  val <- runReaderT query watched
  watchSet <- S.fromList <$> liftIO (readIORef watched)
  pure (validToEither val, watchSet)

-- | Simply runs X query once.
runXQuery :: XPQuery a -> XIO (Either [XQueryError] a)
runXQuery (XPQuery query) = do
  placeholder <- liftIO (newIORef [])
  validToEither <$> runReaderT query placeholder

-- | Watch X query at the given window.
-- Note, allows some modifier process in between.
-- Immediately runs query to get current property right away.
--
-- Only the first query deliver error message,
-- and any other query which fail to produce result doesn't emit the task.
watchXQuery ::
  Window ->
  XPQuery a ->
  (a -> ExceptT [XQueryError] XIO b) ->
  XIO (Either [XQueryError] (Task b))
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
  deriving (Eq, Ord)

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

-- | Request current desktop to change. Must be called from root window.
reqCurrentDesktop :: XIO (Int -> IO ())
reqCurrentDesktop = do
  root <- xWindow
  typCurDesk <- xAtom "_NET_CURRENT_DESKTOP"
  xSendTo structureNotifyMask root $ \event idx -> liftIO $ do
    setEventType event clientMessage
    setClientMessageEvent' event root typCurDesk 32 [fromIntegral idx, fromIntegral currentTime]

getDesktopLayout :: XPQuery T.Text
getDesktopLayout = queryProp @T.Text "_XMONAD_CURRENT_LAYOUT"

data LayoutCmd = NextLayout | ResetLayout
layoutDat = \case
  NextLayout -> 1
  ResetLayout -> -1

-- | Request setting desktop layout. Send 1 for next layout, -1 for default layout.
reqDesktopLayout :: XIO (LayoutCmd -> IO ())
reqDesktopLayout = do
  root <- xWindow
  typLayout <- xAtom "_XMONAD_CURRENT_LAYOUT"
  xSendTo structureNotifyMask root $ \event cmd -> liftIO $ do
    setEventType event clientMessage
    setClientMessageEvent' event root typLayout 32 [layoutDat cmd, fromIntegral currentTime]

-- | Get all windows.
getAllWindows :: XPQuery (V.Vector Window)
getAllWindows = V.fromList <$> queryProp @[Window] "_NET_CLIENT_LIST"

-- | Get an active window.
getActiveWindow :: XPQuery (Maybe Window)
getActiveWindow = find (> 0) <$> queryProp @[Window] "_NET_ACTIVE_WINDOW"

-- | Request active window. flag should be True if you are a pager.
-- Must be called from root window.
reqActiveWindow :: Bool -> XIO (Window -> IO ())
reqActiveWindow flag = do
  root <- xWindow
  typActive <- xAtom "_NET_ACTIVE_WINDOW"
  xSendTo structureNotifyMask root $ \event target -> liftIO $ do
    let srcInd = if flag then 2 else 1
    setEventType event clientMessage
    -- Is this "currentTime" thing right?
    setClientMessageEvent' event target typActive 32 [srcInd, fromIntegral currentTime]

-- | Inclusive states of the window.
data WMStateEx = WinHidden | WinDemandAttention
  deriving (Eq, Ord)

-- TODO WM_CLASS must be [instance name, class name]

-- | Information on the specific window.
data WindowInfo = WindowInfo
  { windowTitle :: !T.Text
  , windowClasses :: !(V.Vector T.Text)
  , windowState :: !(S.Set WMStateEx)
  }

-- | Window information. Only fails when WM_CLASS property is missing.
-- When name(title) is missing, this gives the empty title.
getWindowInfo :: XPQuery WindowInfo
getWindowInfo = WindowInfo <$> wmTitle <*> wmClass <*> wmState
  where
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

-- | Get the desktop certain window resides on.
getWindowDesktop :: XPQuery Int
getWindowDesktop = queryProp @Int "_NET_WM_DESKTOP" <|> pure (-1)

instance XPropType CLong [Gtk.RawIcon] where
  parseProp = \case
    [] -> pure []
    width : height : xs
      | iconWidth <- fromIntegral width
      , iconHeight <- fromIntegral height
      , (dat, rem) <- splitAt (fromIntegral $ iconWidth * iconHeight) xs -> do
          let iconColors = LBS.toStrict . BS.toLazyByteString $ foldMap (BS.word32BE . fromIntegral) dat
          if BS.length iconColors /= fromIntegral (4 * iconWidth * iconHeight)
            then throwError $ T.pack "Not enough pixels read for given width, height"
            else (Gtk.RawIcon{..} :) <$> parseProp rem
    _ -> throwError $ T.pack "Cannot read width, height"

-- | Get the window icon.
-- Since reading it takes time, it is not advised to listen to the property.
getWindowIcon :: XPQuery [Gtk.RawIcon]
getWindowIcon = queryProp @[Gtk.RawIcon] "_NET_WM_ICON"
