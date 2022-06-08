module System.Pulp.PulpEnv (
  PulpEnv,
  PulpIO,
  PulpArg (..),
  defPulpArg,
  runPulpIO,
  pulpXHandle,
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

defPulpArg :: PulpArg
defPulpArg =
  PulpArg
    { loggerFormat = defLogFormat
    , loggerVerbosity = LevelInfo
    }

instance MonadLog PulpIO where
  askLog = PulpIO $ asks (\PulpEnv{pulpLogger} -> pulpLogger)

-- | Run an PulpIO action. Recommended to call only once.
runPulpIO :: PulpArg -> PulpIO a -> IO a
runPulpIO PulpArg{..} (PulpIO act) = do
  logStderr $ \logger -> do
    let pulpLogger = withVerbosity loggerVerbosity $ withFormat loggerFormat logger
    pulpXHandling <- liftIO startXIO
    runReaderT act PulpEnv{..}

-- | Register X handle to the background X11 connection.
pulpXHandle :: XIO () a -> PulpIO a
pulpXHandle initiate = do
  xHandling <- PulpIO . asks $ \PulpEnv{pulpXHandling} -> pulpXHandling
  liftIO $ runXHandling xHandling initiate
