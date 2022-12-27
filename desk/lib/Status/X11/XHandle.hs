module Status.X11.XHandle (
  X11Env (..),
  ActX11 (..),
  liftDWIO,
  XIO,
  xGetExt,
  xWithExt,
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

data XHandle r = XHandle
  { xhDisplay :: !Display
  , xhWindow :: !Window
  , xhListeners :: !(IORef XListeners)
  , xhActQueue :: TQueue (IO ())
  , xhExtends :: !r
  }

newtype XIO r a = XIO (ReaderT (XHandle r) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance ActX11 (XIO r) where
  xDisplay = XIO . asks $ \XHandle{xhDisplay} -> xhDisplay
  xWindow = XIO . asks $ \XHandle{xhWindow} -> xhWindow
  xAtom name = liftDWIO $ \d _ -> liftIO $ internAtom d name False

-- Internal
xListeners :: XIO r (IORef XListeners)
xListeners = XIO . asks $ \XHandle{xhListeners} -> xhListeners

-- Internal
xActQueue :: XIO r (TQueue (IO ()))
xActQueue = XIO . asks $ \XHandle{xhActQueue} -> xhActQueue

-- | Get external.
xGetExt :: XIO r r
xGetExt = XIO . asks $ \XHandle{xhExtends} -> xhExtends

-- | Run on certain window. It is advised not to use this function.
xOnWindow :: Window -> XIO r a -> XIO r a
xOnWindow newWin (XIO act) = XIO (withReaderT withNewWin act)
  where
    withNewWin XHandle{..} = XHandle{xhWindow = newWin, ..}

-- | Run with extension function.
xWithExt :: (r' -> r) -> XIO r a -> XIO r' a
xWithExt extF (XIO act) = XIO (withReaderT withF act)
  where
    withF XHandle{..} = XHandle{xhExtends = extF xhExtends, ..}

-- TODO Prevent waits while running X handling?
newtype XHandling r = XHandling (forall a. XIO r a -> IO a)
runXHandling :: XHandling r -> XIO r a -> IO a
runXHandling (XHandling xHandle) = xHandle

class MonadIO m => MonadXHand m where
  askXHand :: m (XHandling ())
  runXHand :: XIO () a -> m a
  runXHand act = askXHand >>= \handling -> liftIO (runXHandling handling act)

-- | Starts X handler and return X handling to register listeners/senders.
-- NOTE: non-fatal X error is ignored.
startXIO :: IO (XHandling ())
startXIO = do
  theHandling <- newEmptyMVar
  _ <- forkOS . bracket (openDisplay "") closeDisplay $ \xhDisplay -> do
      let xhScreen = defaultScreen xhDisplay
      xhWindow <- rootWindow xhDisplay xhScreen
      xhActQueue <- newTQueueIO
      xhListeners <- newIORef (XListeners M.empty M.empty)
      let runX (XIO act) = runReaderT act XHandle{xhExtends = (), ..}
      runX $ do
        xHandling >>= liftIO . putMVar theHandling
        startingX
  takeMVar theHandling
  where
    startingX = withRunInIO $ \unliftX -> do
      actQueue <- unliftX xActQueue
      allocaXEvent $ \evPtr -> forever $ do
        atomically (flushTQueue actQueue) >>= sequenceA_
        -- .^. Before handling next X event, performs tasks in need of handling.
        unliftX $ listenLeft evPtr
        -- Waits for 1ms, because otherwise we are overloading stuffs
        threadDelay 1000

    listenLeft evPtr = withRunInIO $ \unliftX -> do
      display <- unliftX xDisplay
      listeners <- unliftX xListeners
      eventsQueued display queuedAfterFlush >>= \case
        0 -> pure () -- Nothing in queue
        _ -> do
          nextEvent display evPtr
          event <- getEvent evPtr
          evWindow <- get_Window evPtr

          listens <- getWinListen evWindow <$> readIORef listeners
          for_ listens $ \(XListen _ _ listen) -> listen event
          unliftX $ listenLeft evPtr

-- | Queues a job to execute on next cycle.
xQueueJob :: IO () -> XIO r ()
xQueueJob job = do
  actQueue <- xActQueue
  liftIO $ atomically (writeTQueue actQueue job)

-- MAYBE Use streaming library here

-- | X handling to queue & listen to the jobs. Waits for the result.
xHandling :: XIO r (XHandling r)
xHandling = withRunInIO $ \unliftX -> pure $
  XHandling $ \act -> do
    actQueue <- unliftX xActQueue
    handleResult <- newEmptyMVar
    atomically (writeTQueue actQueue $ unliftX act >>= putMVar handleResult)
    takeMVar handleResult

-- | Take X query commands, give the action to query the result.
-- The query will be performed in the X thread.
-- WARNING: the returned action can block while querying the result.
-- The `query` should not throw.
xQueryOnce :: (a -> XIO r b) -> XIO r (a -> IO b)
xQueryOnce query = do
  hand <- xHandling
  pure $ \arg -> runXHandling hand (query arg)

-- | Listen with certain event mask at certain window.
-- Task variable should be cared of as soon as possible, as it would put the event loop in halt.
-- Begins with optional starting value, emits task when the handling yields something.
xListenTo ::
  EventMask ->
  Window ->
  Maybe a ->
  (Event -> XIO r (Maybe a)) ->
  XIO r (Task a)
xListenTo mask window initial handler = withRunInIO $ \unliftX -> do
  listeners <- unliftX xListeners
  key <- newUnique
  listenQueue <- newTQueueIO
  let writeListen val = atomically $ writeTQueue listenQueue val
  traverse_ writeListen initial
  let listen = XListen mask window $ \event ->
        (unliftX . xOnWindow window) (handler event) >>= traverse_ writeListen
  let beginListen = do
        modifyIORef' listeners $ insertListen key listen
        (unliftX . xOnWindow window) (updateMask listeners)
  let endListen = do
        modifyIORef' listeners $ deleteListen key
        (unliftX . xOnWindow window) (updateMask listeners)
  taskCreate listenQueue (unliftX $ xQueueJob endListen) <$ beginListen
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
  (XEventPtr -> a -> XIO r ()) ->
  XIO r (a -> IO ())
-- MAYBE Further flesh this out when I got time
xSendTo mask window factory = withRunInIO $ \unliftX -> do
  pure $ \arg -> unliftX . xQueueJob $ do
    allocaXEvent $ \event -> unliftX . xOnWindow window $ do
      factory event arg
      liftDWIO $ \d w -> sendEvent d w True mask event
