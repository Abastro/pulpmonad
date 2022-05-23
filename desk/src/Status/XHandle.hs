module Status.XHandle (
  X11Env (..),
  X11,
  runXWith,
  withXExt,
  mapX11,
  onXWindow,
  xAtom,
  selectXEvents,
  xEventLoop,
) where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

data X11Env r = X11Env
  { theDisplay :: !Display
  , targetWindow :: !Window
  , extData :: !r
  }
  deriving (Functor)

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

xAtom :: String -> X11 r Atom
xAtom name = liftDWIO $ \d _ -> internAtom d name False



-- | Select events to listen to for the target window.
selectXEvents :: EventMask -> X11 r ()
selectXEvents mask = liftDWIO $ \d w -> selectInput d w mask

-- | Turn the current thread into event loop handler. Never returns.
xEventLoop :: (Event -> X11 r a) -> X11 r ()
xEventLoop handler = do
  X11Env{theDisplay} <- ask
  unlifts <- askRunInIO
  liftIO $ do
    allocaXEvent $ \evPtr -> forever $ do
      nextEvent theDisplay evPtr
      getEvent evPtr >>= unlifts . handler
