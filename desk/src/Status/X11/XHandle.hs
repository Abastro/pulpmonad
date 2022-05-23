module Status.X11.XHandle (
  X11Env (..),
  ActX11 (..),
  X11,
  runXWith,
  withXExt,
  mapX11,
  onXWindow,
  XEventHandle,
  x11ToEventHandle,
  xRunLoop,
  xListenTo,
  xEventLoop,
  selectXEvents,
) where

import Control.Concurrent
import Control.Concurrent.Task
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Unique
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

data X11Env r = X11Env
  { theDisplay :: !Display
  , targetWindow :: !Window
  , extData :: !r
  }
  deriving (Functor)

-- | Common actions for X11 monad. IO access is NOT assumed.
class Functor m => ActX11 m where
  -- | Gets the X display.
  xDisplay :: m Display

  -- | Gets current/target X window.
  xWindow :: m Window

  -- | Queries certain X Atom.
  xAtom :: String -> m Atom

instance (ActX11 m) => ActX11 (MaybeT m) where
  xDisplay = MaybeT $ Just <$> xDisplay
  xWindow = MaybeT $ Just <$> xWindow
  xAtom name = MaybeT $ Just <$> xAtom name

-- | Monad for X11 handling, with IO monad as base.
newtype X11 r a = X11 (ReaderT (X11Env r) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (X11Env r), MonadUnliftIO)

runXWith :: Display -> Window -> X11 () a -> IO a
runXWith display rwin (X11 act) = runReaderT act (X11Env display rwin ())

withXExt :: (r -> r') -> X11 r' a -> X11 r a
withXExt ext (X11 run) = X11 $ withReaderT (fmap ext) run

mapX11 :: (IO a -> IO b) -> (X11 r a -> X11 r b)
mapX11 f (X11 run) = X11 (mapReaderT f run)

-- | Usually, the X11 monad work on the root window.
-- This one is for running actions for certain target window.
onXWindow :: Window -> X11 r a -> X11 r a
onXWindow window (X11 run) = X11 $ withReaderT (\X11Env{..} -> X11Env{targetWindow = window, ..}) run

liftDWIO :: (Display -> Window -> IO a) -> X11 r a
liftDWIO act = do
  X11Env{theDisplay, targetWindow} <- ask
  liftIO $ act theDisplay targetWindow

instance ActX11 (X11 r) where
  xDisplay = asks (\X11Env{theDisplay} -> theDisplay)
  xWindow = asks (\X11Env{targetWindow} -> targetWindow)
  xAtom name = liftDWIO $ \d _ -> internAtom d name False

{---------------------- X Events ------------------------}

data XListeners = XListeners
  { xListeners :: !(M.Map Unique XListen)
  , perWinListener :: !(M.Map Window (S.Set Unique))
  }

data XListen = XListen !Window (Event -> IO ())

-- Internal
insertListen :: Unique -> XListen -> XListeners -> XListeners
insertListen key listen@(XListen win _) XListeners{..} =
  XListeners
    { xListeners = M.insert key listen xListeners
    , perWinListener = M.alter (Just . S.insert key . notToEmpty) win perWinListener
    }
  where
    notToEmpty = fromMaybe S.empty

-- Internal
deleteListen :: Unique -> XListeners -> XListeners
deleteListen key XListeners{..}
  | (Just (XListen win _), xListeners') <- M.updateLookupWithKey (\_ _ -> Nothing) key xListeners =
      XListeners
        { xListeners = xListeners'
        , perWinListener = M.alter (>>= emptyToNot . S.delete key) win perWinListener
        }
  | otherwise = XListeners{..}
  where
    emptyToNot set = set <$ guard (S.null set)

-- | Monad for X event handling.
newtype XEventHandle a = XEventHandle (X11 (IORef XListeners) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, ActX11)

-- | Lift X11 to Event Handle.
x11ToEventHandle :: r -> X11 r a -> XEventHandle a
x11ToEventHandle ext act = XEventHandle $ withXExt (\_ -> ext) act

-- | Runs into an X event loop, never returns.
-- Note that the parameter is only run once on the root window, to register the listeners.
-- NOTE: Currently, typical non-fatal X error during the loop is ignored.
xRunLoop :: XEventHandle () -> X11 r ()
xRunLoop (XEventHandle initiate) = do
  X11Env{theDisplay} <- ask
  withRunInIO $ \unliftX -> do
    listeners <- newIORef (XListeners M.empty M.empty)
    -- Is this really experimental as docs say? Let's find out.
    setErrorHandler $ \_disp _errEvent -> pure ()
    unliftX $ withXExt (\_ -> listeners) initiate
    -- Registers the listeners
    allocaXEvent $ \evPtr -> forever $ do
      nextEvent theDisplay evPtr
      event <- getEvent evPtr
      -- While this says get_Window, it actually grabs `event` for MapNotify/UnmapNotify.
      -- This allows the substructure handling.
      window <- get_Window evPtr
      XListeners{..} <- readIORef listeners
      let perWindow = maybe [] S.toList $ perWinListener M.!? window
      let listens = mapMaybe (xListeners M.!?) perWindow
      for_ listens $ \(XListen _ listen) -> listen event

-- | Listen with certain event mask at certain window.
-- Task variable should be cared of as soon as possible, as it would put the event loop in halt.
-- Begins with optional starting value, emits task when the handling yields something.
xListenTo ::
  EventMask ->
  Window ->
  (Maybe a) ->
  (Event -> XEventHandle (Maybe a)) ->
  XEventHandle (Task a)
xListenTo mask window initial handler = XEventHandle $ do
  X11Env{extData = listeners} <- ask
  withRunInIO $ \unliftListen -> do
    key <- newUnique
    taskVar <- maybe newEmptyMVar newMVar initial
    let listen = XListen window $ \event -> do
          let XEventHandle handle = handler event
          unliftListen (onXWindow window handle) >>= traverse_ (putMVar taskVar)
    let beginListen = do
          modifyIORef' listeners $ insertListen key listen
          unliftListen . liftDWIO $ \d w -> selectInput d w mask
    let endListen = do
          modifyIORef' listeners $ deleteListen key
          unliftListen . liftDWIO $ \d w -> selectInput d w 0
    Task{killTask = endListen, taskVar} <$ beginListen
  where

-- | Select events to listen to for the target window.
selectXEvents :: EventMask -> X11 r ()
selectXEvents mask = liftDWIO $ \d w -> selectInput d w mask

-- | Turn the current thread into event loop handler. Never returns.
xEventLoop :: (Event -> X11 r a) -> X11 r ()
xEventLoop handler = do
  X11Env{theDisplay} <- ask
  withRunInIO $ \unlifts -> do
    allocaXEvent $ \evPtr -> forever $ do
      nextEvent theDisplay evPtr
      getEvent evPtr >>= unlifts . handler
