module Status.X11.XHandle (
  X11Env (..),
  ActX11 (..),
  liftDWIO,
  XIO,
  xGetExt,
  xWithExt,
  xOnWindow,
  startXIO,
  xQueueJob,
  xListenTo,
  xListenTo_,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Task
import Control.Exception
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
import Data.Bits
import Data.Functor.Compose

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

-- | Starts X handler using initiate action.
-- NOTE: non-fatal X error is ignored.
startXIO :: XIO () a -> IO a
startXIO initiate = do
  -- TODO Initiation action should NOT be XIO
  initResult <- newEmptyMVar

  _ <- forkIO . bracket (openDisplay "") closeDisplay $ \xhDisplay -> do
    let xhScreen = defaultScreen xhDisplay
    xhWindow <- rootWindow xhDisplay xhScreen
    xhListeners <- newIORef (XListeners M.empty M.empty)
    xhActQueue <- newTQueueIO
    let runX (XIO act) = runReaderT act XHandle{xhExtends = (), ..}
    runX $ do
      initiate >>= liftIO . putMVar initResult
      startingX
  takeMVar initResult
  where
    startingX = withRunInIO $ \unliftX -> do
      display <- unliftX xDisplay
      listeners <- unliftX xListeners
      actQueue <- unliftX xActQueue

      setErrorHandler $ \_disp _errEvent -> pure ()
      allocaXEvent $ \evPtr -> forever $ do
        atomically (flushTQueue actQueue) >>= traverse_ id
        -- .^. Before handling next X event, performs tasks in need of handling.
        nextEvent display evPtr
        event <- getEvent evPtr
        -- Hope this grabs `event` for MapNotify/UnmapNotify,
        -- which allows for the substructure handling.
        evWindow <- get_Window evPtr

        listens <- getWinListen evWindow <$> readIORef listeners
        for_ listens $ \(XListen _ _ listen) -> listen event

-- | Queues a job to execute on next cycle.
xQueueJob :: XIO r () -> XIO r ()
xQueueJob job = do
  actQueue <- xActQueue
  withRunInIO $ \unliftX -> atomically (writeTQueue actQueue $ unliftX job)

-- MAYBE Use streaming library here

-- | Listen with certain event mask at certain window.
-- Task variable should be cared of as soon as possible, as it would put the event loop in halt.
-- Begins with optional starting value, emits task when the handling yields something.
xListenTo ::
  EventMask ->
  Window ->
  (Maybe a) ->
  (Event -> XIO r (Maybe a)) ->
  XIO r (Task a)
xListenTo mask window initial handler = withRunInIO $ \unliftX -> do
  listeners <- unliftX xListeners
  key <- newUnique
  listenQueue <- newTQueueIO
  let writeListen val = atomically $ writeTQueue listenQueue val
  traverse_ writeListen initial
  let listen = XListen mask window $ \event ->
        unliftX (xOnWindow window $ handler event) >>= traverse_ writeListen
  let beginListen = do
        modifyIORef' listeners $ insertListen key listen
        unliftX $ updateMask listeners
  let endListen = do
        modifyIORef' listeners $ deleteListen key
        unliftX $ updateMask listeners
  let killTask = unliftX $ xQueueJob (liftIO endListen)
  taskCreate listenQueue killTask <$ beginListen
  where
    updateMask listeners = liftDWIO $ \disp win -> do
      Ior newMask <- foldMap (Ior . maskOf) . getWinListen win <$> readIORef listeners
      selectInput disp win newMask
    maskOf (XListen mask _ _) = mask

-- | Similar to xListenTo, but does not emit/report any value to the Task.
-- NB: Subsequently, it is also impossible to stop listening on this one.
xListenTo_ ::
  EventMask ->
  Window ->
  (Event -> XIO r ()) ->
  XIO r ()
xListenTo_ mask window handler = withRunInIO $ \unliftX -> do
  listeners <- unliftX xListeners
  key <- newUnique
  let listen = XListen mask window $ \event -> do
        unliftX (xOnWindow window $ handler event)
  modifyIORef' listeners $ insertListen key listen
  unliftX . liftDWIO $ \d w -> selectInput d w mask
