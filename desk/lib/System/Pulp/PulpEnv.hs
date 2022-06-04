module System.Pulp.PulpEnv (
  PulpEnv,
  PulpIO,
  runPulpIO,
  pulpXHandle,
) where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Status.X11.XHandle
import System.Log.FastLogger
import System.Log.LogPrint

-- | Pulp environment.
data PulpEnv = PulpEnv
  { pulpLogger :: !FastLogger
  , pulpXHandling :: !(XHandling ())
  }

newtype PulpIO a = PulpIO (ReaderT PulpEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance MonadLog PulpIO where
  askLog = withF defLogFormat <$> (PulpIO . asks) (\PulpEnv{pulpLogger} -> pulpLogger)

-- | Run an PulpIO action. Recommended to call only once.
runPulpIO :: PulpIO a -> IO a
runPulpIO (PulpIO act) = do
  logStderr $ \pulpLogger -> do
    pulpXHandling <- liftIO startXIO
    runReaderT act PulpEnv{..}

-- | Register X handle to the background X11 connection.
pulpXHandle :: XIO () a -> PulpIO a
pulpXHandle initiate = do
  xHandling <- PulpIO . asks $ \PulpEnv{pulpXHandling} -> pulpXHandling
  liftIO $ runXHandling xHandling initiate
