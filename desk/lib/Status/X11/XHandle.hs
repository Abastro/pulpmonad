module Status.X11.XHandle (
  ActX11 (..),
  liftDWIO,
  XIO,
  xOnWindow,
  withXHook,
  XHook,
  MonadXHook (..),
  runXHook,
  xQueryOnce,
  xListenTo,
  xSendTo,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Task
import Control.Exception
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Functor.Compose
import Data.IORef
import Data.Unique
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Status.X11.XListen

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

instance (ActX11 m) => ActX11 (ReaderT r m) where
  xDisplay = ReaderT $ const xDisplay
  xWindow = ReaderT $ const xWindow
  xAtom name = ReaderT $ const (xAtom name)

instance (ActX11 m, Applicative f) => ActX11 (Compose m f) where
  xDisplay = Compose $ pure <$> xDisplay
  xWindow = Compose $ pure <$> xWindow
  xAtom name = Compose $ pure <$> xAtom name

-- | Lift IO action involving display & window.
liftDWIO :: (MonadIO m, ActX11 m) => (Display -> Window -> IO a) -> m a
liftDWIO io = do
  disp <- xDisplay
  win <- xWindow
  liftIO $ io disp win

{-------------------------------------------------------------------
                            XIO Monad
--------------------------------------------------------------------}

data XHandle = XHandle
  { display :: !Display
  , window :: !Window
  , listeners :: !(IORef XListeners)
  , actQueue :: TQueue (IO ())
  -- ^ Queue to execute actions on next cycle.
  }

newtype XIO a = XIO (ReaderT XHandle IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance ActX11 XIO where
  xDisplay = XIO . asks $ \handle -> handle.display
  xWindow = XIO . asks $ \handle -> handle.window
  xAtom name = liftDWIO $ \d _ -> liftIO $ internAtom d name False

-- Internal
runXIO :: XHandle -> XIO a -> IO a
runXIO handle (XIO act) = runReaderT act handle

-- | Run on certain window. It is advised not to directly use this function.
xOnWindow :: Window -> XIO a -> XIO a
xOnWindow newWin (XIO act) = XIO (withReaderT withNewWin act)
  where
    withNewWin handle = handle{window = newWin}

-- TODO Prevent waits while running X handling?
newtype XHook = XHook (forall a. XIO a -> IO a)

class MonadIO m => MonadXHook m where
  askXHook :: m XHook

-- | Hook into X and run the action on the thread.
runXHook :: MonadXHook m => XIO a -> m a
runXHook act = askXHook >>= \(XHook hook) -> liftIO (hook act)

-- | Starts X handler and provides X handling to the action.
--
-- The X handling allows to register listeners and senders from any thread.
--
-- NOTE: non-fatal X errors are ignored.
withXHook :: (XHook -> IO a) -> IO a
withXHook runWith = do
  hookVar <- newEmptyMVar
  bracket (xHandlerThread hookVar) killThread $ \_ -> do
    runWith =<< takeMVar hookVar

-- | Spawns X handler thread.
xHandlerThread :: MVar XHook -> IO ThreadId
xHandlerThread hookVar = forkOS . bracket (openDisplay "") closeDisplay $ \display -> do
  handle <- createHandle display
  putMVar hookVar (getXHook handle)
  xHandlerLoop handle

createHandle :: Display -> IO XHandle
createHandle display = do
  let screen = defaultScreen display
  window <- rootWindow display screen
  listeners <- newIORef newXListeners
  actQueue <- newTQueueIO
  pure XHandle{..}

xHandlerLoop :: XHandle -> IO ()
xHandlerLoop handle = allocaXEvent $ \evPtr -> forever $ do
  atomically (flushTQueue handle.actQueue) >>= sequenceA_
  -- .^. Before handling next X event, performs tasks in need of handling.
  listenRemaining evPtr
  -- Waits for 1ms, because otherwise we are overloading CPU
  threadDelay 1000
  where
    listenRemaining evPtr = fix $ \recurse -> do
      numQueued <- eventsQueued handle.display queuedAfterFlush
      when (numQueued > 0) $ do
        nextEvent handle.display evPtr
        event <- getEvent evPtr
        evWindow <- get_Window evPtr

        listens <- listensForWindow evWindow <$> readIORef handle.listeners
        for_ listens $ \listen -> listen.onEvent event
        recurse

-- | Converts XHandle to XHandling (implicit unlifting).
getXHook :: XHandle -> XHook
getXHook handle = XHook $ \act -> do
  handleResult <- newEmptyMVar
  let action = runXIO handle act >>= putMVar handleResult
  atomically (writeTQueue handle.actQueue action)
  takeMVar handleResult

-- | Take X query commands, give the action to query the result.
-- The query will be performed in the X thread.
-- WARNING: the returned action can block while querying the result.
-- The `query` should not throw.
xQueryOnce :: (a -> XIO b) -> XIO (a -> IO b)
xQueryOnce query = do
  XHook hook <- getXHook <$> XIO ask
  pure $ \arg -> hook (query arg)

-- | Listen with certain event mask at certain window.
-- Task variable should be cared of as soon as possible, as it would put the event loop in halt.
-- Begins with optional starting value, emits task when the handling yields something.
xListenTo ::
  EventMask ->
  Window ->
  Maybe a ->
  (Event -> XIO (Maybe a)) ->
  XIO (Task a)
xListenTo mask window initial handler = do
  handle <- XIO ask
  liftIO $ do
    key <- newUnique
    listenQueue <- newTQueueIO
    let writeListen val = atomically $ writeTQueue listenQueue val
    traverse_ writeListen initial
    let listen = MkXListen mask $ \event ->
          runXIO handle (xOnWindow window $ handler event) >>= traverse_ writeListen
    let beginListen = do
          modifyIORef' handle.listeners $ insertListen window key listen
          runXIO handle (updateMask handle.listeners)
    let endListen = do
          modifyIORef' handle.listeners $ deleteListen window key
          runXIO handle (updateMask handle.listeners)
    taskCreate listenQueue (atomically $ writeTQueue handle.actQueue endListen) <$ beginListen
  where
    updateMask listeners = liftDWIO $ \disp _ -> do
      newMask <- maskFor window <$> readIORef listeners
      selectInput disp window newMask

-- | Send event with certain mask to certain window, through the X11 thread.
--
-- Note that the third parameter is used to specify the event.
-- During the specifying process, the target window is set to the second parameter.
--
-- This one does not support event propagation.
xSendTo ::
  EventMask ->
  Window ->
  (XEventPtr -> a -> XIO ()) ->
  XIO (a -> IO ())
-- MAYBE Further flesh this out when I got time
xSendTo mask window factory = do
  handle <- XIO ask
  pure $ \arg -> atomically $ writeTQueue handle.actQueue $ do
    -- This action is being queued for later.
    allocaXEvent $ \event -> runXIO handle . xOnWindow window $ do
      factory event arg
      liftDWIO $ \d w -> sendEvent d w True mask event
