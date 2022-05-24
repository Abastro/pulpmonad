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
  xListenTo_,
) where

import Control.Concurrent
import Control.Concurrent.STM
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

{-------------------------------------------------------------------
                        X Event Handling
--------------------------------------------------------------------}

data XListeners = XListeners
  { xListens :: !(M.Map Unique XListen)
  , perWinListens :: !(M.Map Window (S.Set Unique))
  }

data XListen = XListen !Window (Event -> IO ())

-- Internal
insertListen :: Unique -> XListen -> XListeners -> XListeners
insertListen key listen@(XListen win _) XListeners{..} =
  XListeners
    { xListens = M.insert key listen xListens
    , perWinListens = M.alter (Just . S.insert key . notToEmpty) win perWinListens
    }
  where
    notToEmpty = fromMaybe S.empty

-- Internal
deleteListen :: Unique -> XListeners -> XListeners
deleteListen key XListeners{..}
  | (Just (XListen win _), xListens') <- M.updateLookupWithKey (\_ _ -> Nothing) key xListens =
      XListeners
        { xListens = xListens'
        , perWinListens = M.alter (>>= emptyToNot . S.delete key) win perWinListens
        }
  | otherwise = XListeners{..}
  where
    emptyToNot set = set <$ guard (S.null set)

data XEvHandleEnv = XEvHandleEnv {listeners :: IORef XListeners, actQueue :: TQueue (IO ())}

-- | Monad for X event handling, should only belong to a single thread.
newtype XEventHandle a = XEventHandle (X11 XEvHandleEnv a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, ActX11)

-- | Lift X11 to Event Handle.
x11ToEventHandle :: r -> X11 r a -> XEventHandle a
x11ToEventHandle ext act = XEventHandle $ withXExt (\_ -> ext) act

-- | Sets up and forks/runs the X event loop action.
-- Note that the parameter is only run once on the root window, to register the listeners.
-- NOTE: Currently, typical non-fatal X error during the loop is ignored.
xRunLoop :: XEventHandle a -> X11 r a
xRunLoop (XEventHandle initiate) = do
  X11Env{theDisplay} <- ask
  withRunInIO $ \unliftX -> do
    -- Varible to pass around initiation result
    initResult <- newEmptyMVar

    forkIO $ do
      -- Initiate listening.
      listeners <- newIORef (XListeners M.empty M.empty)
      actQueue <- newTQueueIO
      putMVar initResult =<< unliftX (withXExt (\_ -> XEvHandleEnv{..}) initiate)

      -- Is this really experimental as docs say? Let's find out.
      setErrorHandler $ \_disp _errEvent -> pure ()
      allocaXEvent $ \evPtr -> forever $ do
        atomically (flushTQueue actQueue) >>= traverse_ id
        -- .^. Before handling next event, look if some tasks are in need of handling.
        nextEvent theDisplay evPtr
        event <- getEvent evPtr
        -- Hope this grabs `event` for MapNotify/UnmapNotify, which allows for the substructure handling.
        window <- get_Window evPtr
        XListeners{..} <- readIORef listeners
        let perWindow = maybe [] S.toList $ perWinListens M.!? window
        let listens = mapMaybe (xListens M.!?) perWindow
        for_ listens $ \(XListen _ listen) -> listen event

    readMVar initResult

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
  X11Env{extData = XEvHandleEnv{..}} <- ask
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
    -- Kill task redirects it to the task.
    Task{killTask = atomically $ writeTQueue actQueue endListen, taskVar} <$ beginListen

-- | Similar to xListenTo, but does not emit/report any value to the Task.
-- NB: Subsequently, it is also impossible to stop listening on this one.
xListenTo_ ::
  EventMask ->
  Window ->
  (Event -> XEventHandle ()) ->
  XEventHandle ()
xListenTo_ mask window handler = XEventHandle $ do
  X11Env{extData = XEvHandleEnv{listeners}} <- ask
  withRunInIO $ \unliftListen -> do
    key <- newUnique
    let listen = XListen window $ \event -> do
          let XEventHandle handle = handler event
          unliftListen (onXWindow window handle)
    modifyIORef' listeners $ insertListen key listen
    unliftListen . liftDWIO $ \d w -> selectInput d w mask
