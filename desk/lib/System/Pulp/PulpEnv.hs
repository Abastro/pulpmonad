module System.Pulp.PulpEnv (
  PulpEnv,
  PulpIO,
  PulpArg (..),
  runPulpIO,
) where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Status.X11.XHandle
import System.Log.LogPrint

-- | Pulp environment.
data PulpEnv = PulpEnv
  { pulpLogger :: !LevelLogger
  , pulpXHandling :: !(XHandling ())
  }

newtype PulpIO a = PulpIO (ReaderT PulpEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

data PulpArg = PulpArg
  { loggerFormat :: LogFormat
  , loggerVerbosity :: !LogLevel
  }

instance MonadLog PulpIO where
  askLog = PulpIO $ asks (\PulpEnv{pulpLogger} -> pulpLogger)

instance MonadXHand PulpIO where
  askXHand = PulpIO $ asks (\PulpEnv{pulpXHandling} -> pulpXHandling)

-- | Run an PulpIO action. Recommended to call only once.
runPulpIO :: PulpArg -> PulpIO a -> IO a
runPulpIO PulpArg{..} (PulpIO act) = do
  logStderr $ \logger -> do
    let pulpLogger = withVerbosity loggerVerbosity $ withFormat loggerFormat logger
    pulpXHandling <- liftIO startXIO
    runReaderT act PulpEnv{..}
