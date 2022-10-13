module Control.Event.Entry (
  Source,
  Sink,
  Handler,
  sourceSimple,
  sourceWithUnreg,
  sourceSink,
) where

import Control.Event.Handler

-- | Source receives a callback, and send the data to callback when signal occurs.
type Source a = AddHandler a

-- | Sink is, effectively, a callback.
type Sink a = Handler a

-- | Source with capability of unregistering handlers.
sourceWithUnreg :: (Handler a -> IO (IO ())) -> Source a
sourceWithUnreg = AddHandler

-- | Source without capability of unregistering handlers.
sourceSimple :: (Handler a -> IO ()) -> Source a
sourceSimple src = sourceWithUnreg $ \handler -> pure () <$ src handler

-- | Source paired with sink to call
sourceSink :: IO (Source a, Sink a)
sourceSink = newAddHandler
