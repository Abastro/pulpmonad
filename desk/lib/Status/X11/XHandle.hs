module Status.X11.XHandle (
  ActX11 (..),
  liftDWIO,
  XIO,
  xOnWindow,
  startXIO,
  XHandling,
  runXHandling,
  MonadXHand (..),
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
import Data.Bits
import Data.Foldable
import Data.Functor.Compose
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Unique
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

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
                            Listeners
--------------------------------------------------------------------}

data XListeners = XListeners
  { xListens :: !(M.Map Unique XListen)
  , perWinListens :: !(M.Map Window (S.Set Unique))
  }

data XListen = XListen !EventMask !Window (Event -> IO ())

-- Internal
getWinListen :: Window -> XListeners -> [XListen]
getWinListen window XListeners{..} =
  mapMaybe (xListens M.!?) $ maybe [] S.toList $ perWinListens M.!? window

-- Internal
insertListen :: Unique -> XListen -> XListeners -> XListeners
insertListen key listen@(XListen _ win _) XListeners{..} =
  XListeners
    { xListens = M.insert key listen xListens
    , perWinListens = M.alter (Just . S.insert key . notToEmpty) win perWinListens
    }
  where
    notToEmpty = fromMaybe S.empty

-- Internal
deleteListen :: Unique -> XListeners -> XListeners
deleteListen key XListeners{..}
  | (Just (XListen _ win _), xListens') <- M.updateLookupWithKey (\_ _ -> Nothing) key xListens =
      XListeners
        { xListens = xListens'
        , perWinListens = M.alter (>>= emptyToNot . S.delete key) win perWinListens
        }
  | otherwise = XListeners{..}
  where
    emptyToNot set = set <$ guard (S.null set)

{-------------------------------------------------------------------
                            XIO Monad
--------------------------------------------------------------------}

data XHandle = XHandle
  { display :: !Display
  , window :: !Window
  , listeners :: !(IORef XListeners)
  , actQueue :: TQueue (IO ())
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

-- Internal
xActQueue :: XIO (TQueue (IO ()))
xActQueue = XIO . asks $ \handle -> handle.actQueue

-- | Run on certain window. It is advised not to directly use this function.
xOnWindow :: Window -> XIO a -> XIO a
xOnWindow newWin (XIO act) = XIO (withReaderT withNewWin act)
  where
    withNewWin handle = handle{window = newWin}

-- TODO Prevent waits while running X handling?
newtype XHandling = XHandling (forall a. XIO a -> IO a)
runXHandling :: XHandling -> XIO a -> IO a
runXHandling (XHandling xHandle) = xHandle

class MonadIO m => MonadXHand m where
  askXHand :: m XHandling
  runXHand :: XIO a -> m a
  runXHand act = askXHand >>= \handling -> liftIO (runXHandling handling act)

-- | Starts X handler and return X handling to register listeners/senders.
-- NOTE: non-fatal X error is ignored.
startXIO :: IO XHandling
startXIO = do
  theHandling <- newEmptyMVar
  _ <- forkOS . bracket (openDisplay "") closeDisplay $ \display -> do
    handle <- createHandle display
    putMVar theHandling (getXHandling handle)
    xHandlerLoop handle
  takeMVar theHandling

createHandle :: Display -> IO XHandle
createHandle display = do
  let screen = defaultScreen display
  window <- rootWindow display screen
  listeners <- newIORef (XListeners M.empty M.empty)
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

        listens <- getWinListen evWindow <$> readIORef handle.listeners
        for_ listens $ \(XListen _ _ listen) -> listen event
        recurse

-- | Converts XHandle to XHandling (implicit unlifting).
getXHandling :: XHandle -> XHandling
getXHandling handle = XHandling $ \act -> do
  handleResult <- newEmptyMVar
  let action = runXIO handle act >>= putMVar handleResult
  atomically (writeTQueue handle.actQueue action)
  takeMVar handleResult

-- | Queues a job to execute on next cycle.
xQueueJob :: IO () -> XIO ()
xQueueJob job = do
  actQueue <- xActQueue
  liftIO $ atomically (writeTQueue actQueue job)

-- | Take X query commands, give the action to query the result.
-- The query will be performed in the X thread.
-- WARNING: the returned action can block while querying the result.
-- The `query` should not throw.
xQueryOnce :: (a -> XIO b) -> XIO (a -> IO b)
xQueryOnce query = do
  handle <- XIO ask
  pure $ \arg -> runXHandling (getXHandling handle) (query arg)

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
    let listen = XListen mask window $ \event ->
          (runXIO handle . xOnWindow window) (handler event) >>= traverse_ writeListen
    let beginListen = do
          modifyIORef' handle.listeners $ insertListen key listen
          (runXIO handle . xOnWindow window) (updateMask handle.listeners)
    let endListen = do
          modifyIORef' handle.listeners $ deleteListen key
          (runXIO handle . xOnWindow window) (updateMask handle.listeners)
    taskCreate listenQueue (runXIO handle $ xQueueJob endListen) <$ beginListen
  where
    updateMask listeners = liftDWIO $ \disp win -> do
      newMask <- foldl' (.|.) 0 . fmap maskOf . getWinListen win <$> readIORef listeners
      selectInput disp win newMask
    maskOf (XListen mask _ _) = mask

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
  pure $ \arg -> runXIO handle . xQueueJob $ do
    allocaXEvent $ \event -> runXIO handle . xOnWindow window $ do
      factory event arg
      liftDWIO $ \d w -> sendEvent d w True mask event
