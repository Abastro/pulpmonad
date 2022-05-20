-- | Status hook for Extended Window Manager Hints.
-- Consult <https://specifications.freedesktop.org/wm-spec/wm-spec-1.3.html>.
module Status.EWMHStatus (
  DesktopID (..),
  WMStateEx (..),
  getWinStateEx,
) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Foreign.C.Types
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types
import XMonad.Core
import XMonad.ManageHook (liftX)

-- TODO: https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.5.html

getWinPropWith :: (Display -> Atom -> Window -> IO a) -> Atom -> Query a
getWinPropWith getter prop = do
  win <- ask
  liftX . withDisplay $ \display -> io $ getter display prop win

getWinProp8 = getWinPropWith getWindowProperty8
getWinProp16 = getWinPropWith getWindowProperty16
getWinProp32 = getWinPropWith getWindowProperty32
getWinPropBytes prop = fmap (BS.pack . map fromIntegral) <$> getWinProp8 prop

onRoot :: Query a -> X a
onRoot query = asks theRoot >>= runQuery query

mayUtf8 :: BS.ByteString -> Maybe T.Text
mayUtf8 = either (const Nothing) Just . T.decodeUtf8'

newtype DesktopID = DesktopID Int

atom_CLIENT_LIST :: X Atom
atom_CLIENT_LIST = getAtom "_NET_CLIENT_LIST"

atom_NUMBER_OF_DESKTOPS :: X Atom
atom_NUMBER_OF_DESKTOPS = getAtom "_NET_NUMBER_OF_DESKTOPS"

-- | Get current desktop (workspace).
getCurrentDesktop :: X DesktopID
getCurrentDesktop = do
  propCurDesk <- getAtom "_NET_CURRENT_DESKTOP"
  mayID <- maybe Nothing listToMaybe <$> onRoot (getWinProp32 propCurDesk)
  pure . DesktopID $ maybe (-1) fromIntegral mayID

getVisibleDesktops :: X [DesktopID] -- How to, hmm
getVisibleDesktops = do
  propVisDesk <- getAtom "_XMONAD_VISIBLE_WORKSPACES"
  undefined

getDesktopNames :: X [T.Text]
getDesktopNames = do
  propDeskNames <- getAtom "_NET_DESKTOP_NAMES"
  bytes <- onRoot (getWinPropBytes propDeskNames)
  pure $ fromMaybe [] (bytes >>= traverse mayUtf8 . BS.split 0)

-- NOTE: Icon not loading is due to /usr/share/pixmaps not being loaded
atom_WM_DESKTOP :: X Atom
atom_WM_DESKTOP = getAtom "_NET_WM_DESKTOP"

atom_WM_CLASS, atom_WM_NAME_EX, atom_WM_NAME, atom_WM_ICON :: X Atom
atom_WM_CLASS = getAtom "WM_CLASS"
atom_WM_NAME_EX = getAtom "_NET_WM_NAME"
atom_WM_NAME = getAtom "WM_NAME"
atom_WM_ICON = getAtom "_NET_WM_ICON"

atom_ACTIVE_WINDOW :: X Atom
atom_ACTIVE_WINDOW = getAtom "_NET_ACTIVE_WINDOW"

getWinTitle :: Query T.Text
getWinTitle = do
  let getWinPropTxt prop = (>>= mayUtf8) <$> getWinPropBytes prop
  propNameExt <- liftX $ getAtom "_NET_WM_NAME"
  propName <- liftX $ getAtom "WM_NAME"
  mayTitle <- runMaybeT $ MaybeT (getWinPropTxt propNameExt) <|> MaybeT (getWinPropTxt propName)
  pure $ fromMaybe T.empty mayTitle

--getWinClass :: Query T.Text


data WMStateEx = WindowHidden

wmStateMap :: X (M.Map CLong WMStateEx)
wmStateMap = do
  hidden <- (,WindowHidden) . fromIntegral <$> getAtom "_NET_WM_STATE_HIDDEN"
  pure $ M.fromList [hidden]

getWinStateEx :: Query [WMStateEx]
getWinStateEx = do
  propWMSt <- liftX $ getAtom "_NET_WM_STATE"
  stMap <- liftX $ wmStateMap
  mapMaybe (stMap M.!?) . fromMaybe [] <$> getWinProp32 propWMSt
