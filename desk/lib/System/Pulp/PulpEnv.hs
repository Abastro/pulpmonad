module System.Pulp.PulpEnv (
  PulpEnv
  , PulpIO
  , PulpArg (..)
  , runPulpIO
  , MonadPulpPath (..)
) where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Status.X11.XHandle
import System.FilePath
import System.Log.LogPrint

-- | Pulp environment.
data PulpEnv = MkPulpEnv
  { pulpLogger :: !LevelLogger
  , pulpXHandling :: !(XHandling ())
  , penvDataDir :: !FilePath
  }

newtype PulpIO a = PulpIO (ReaderT PulpEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

data PulpArg = MkPulpArg
  { loggerFormat :: LogFormat
  , loggerVerbosity :: !LogLevel
  , argDataDir :: !String
  }

instance MonadLog PulpIO where
  askLog = PulpIO $ asks (\MkPulpEnv{pulpLogger} -> pulpLogger)

instance MonadXHand PulpIO where
  askXHand = PulpIO $ asks (\MkPulpEnv{pulpXHandling} -> pulpXHandling)

class Monad m => MonadPulpPath m where
  -- | Path of certain data within the data directory.
  pulpDataPath :: FilePath -> m FilePath

instance MonadPulpPath PulpIO where
  pulpDataPath path = PulpIO $ asks (\MkPulpEnv{penvDataDir} -> penvDataDir </> path)

-- | Run an PulpIO action. Recommended to call only once.
runPulpIO :: PulpArg -> PulpIO a -> IO a
runPulpIO MkPulpArg{..} (PulpIO act) = logStderr $ \logger -> do
  let pulpLogger = withVerbosity loggerVerbosity $ withFormat loggerFormat logger
  pulpXHandling <- liftIO startXIO
  runReaderT act MkPulpEnv{penvDataDir = argDataDir, ..}
