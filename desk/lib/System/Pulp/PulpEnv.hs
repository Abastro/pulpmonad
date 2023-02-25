module System.Pulp.PulpEnv (
  PulpEnv
  , PulpIO
  , PulpArg (..)
  , runPulpIO
) where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Status.X11.XHandle
import System.Log.LogPrint
import System.Pulp.PulpPath

-- | Pulp environment.
data PulpEnv = MkPulpEnv
  { pulpLogger :: !LevelLogger
  , pulpXHandling :: !XHandling
  }

newtype PulpIO a = PulpIO (ReaderT PulpEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

data PulpArg = MkPulpArg
  { loggerFormat :: LogFormat
  , loggerVerbosity :: !LogLevel
  , argDataDir :: !String
  }

instance MonadLog PulpIO where
  askLog :: PulpIO LevelLogger
  askLog = PulpIO $ asks (\env -> env.pulpLogger)

instance MonadXHand PulpIO where
  askXHand :: PulpIO XHandling
  askXHand = PulpIO $ asks (\env -> env.pulpXHandling)

-- | Run an PulpIO action. Recommended to call only once.
runPulpIO :: PulpArg -> PulpIO a -> IO a
runPulpIO MkPulpArg{..} (PulpIO act) = logStderr $ \logger -> do
  -- Path is set as global
  initDataDir argDataDir
  let pulpLogger = withVerbosity loggerVerbosity $ withFormat loggerFormat logger
  withXHandling $ \pulpXHandling -> do
    runReaderT act MkPulpEnv{..}
