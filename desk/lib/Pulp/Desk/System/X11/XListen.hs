-- TODO Testing
module Pulp.Desk.System.X11.XListen (
  XListeners,
  newXListeners,
  XListen (..),
  listensForWindow,
  insertListen,
  deleteListen,
  maskFor,
) where

import Control.Monad
import Data.Bits
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Unique
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras

newtype XListeners = MkXListeners (M.Map Window (M.Map Unique XListen))

data XListen = MkXListen
  { mask :: !EventMask
  , onEvent :: Event -> IO ()
  -- onEvent should be @Event -> XIO ()@,
  -- but this introduces cyclic dependency between modules.
  }

newXListeners :: XListeners
newXListeners = MkXListeners M.empty

listensForWindow :: Window -> XListeners -> [XListen]
listensForWindow window (MkXListeners listens) = maybe [] M.elems $ listens M.!? window

maskFor :: Window -> XListeners -> EventMask
maskFor window = getIor . foldMap' getMask . listensForWindow window
  where
    getMask listen = Ior (listen.mask)

nothingToEmpty :: Maybe (M.Map k a) -> M.Map k a
nothingToEmpty = fromMaybe M.empty

emptyToNothing :: M.Map k a -> Maybe (M.Map k a)
emptyToNothing map = map <$ guard (M.null map)

insertListen :: Window -> Unique -> XListen -> XListeners -> XListeners
insertListen window key listen (MkXListeners old) = MkXListeners new
  where
    new = M.alter updPerWin window old
    updPerWin = Just . M.insert key listen . nothingToEmpty

deleteListen :: Window -> Unique -> XListeners -> XListeners
deleteListen window key (MkXListeners old) = MkXListeners new
  where
    new = M.alter updPerWin window old
    updPerWin = emptyToNothing . M.delete key . nothingToEmpty
