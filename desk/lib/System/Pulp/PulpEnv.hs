module System.Pulp.PulpEnv (
  PulpEnv,
  PulpIO,
  PulpArg (..),
  runPulpIO,
  PulpDir (..),
  MonadPulpPath (..),
) where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Status.X11.XHandle
import System.Environment
import System.FilePath
import System.Log.LogPrint

-- | Pulp environment.
data PulpEnv = PulpEnv
  { pulpLogger :: !LevelLogger
  , pulpXHandling :: !(XHandling ())
  , pulpDataDir :: !FilePath
  }

newtype PulpIO a = PulpIO (ReaderT PulpEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

data PulpArg = PulpArg
  { loggerFormat :: LogFormat
  , loggerVerbosity :: !LogLevel
  , dataDirEnv :: !String
  }

instance MonadLog PulpIO where
  askLog = PulpIO $ asks (\PulpEnv{pulpLogger} -> pulpLogger)

instance MonadXHand PulpIO where
  askXHand = PulpIO $ asks (\PulpEnv{pulpXHandling} -> pulpXHandling)

data PulpDir = PulpUI | PulpAsset | PulpStyle

class Monad m => MonadPulpPath m where
  -- | Gets the file location with given designation.
  pulpFile :: PulpDir -> FilePath -> m FilePath

instance MonadPulpPath PulpIO where
  pulpFile :: PulpDir -> FilePath -> PulpIO FilePath
  pulpFile typ path = PulpIO $ do
    PulpEnv{pulpDataDir} <- ask
    pure $ case typ of
      PulpUI -> pulpDataDir </> "ui" </> path
      PulpAsset -> pulpDataDir </> "asset" </> path
      PulpStyle -> pulpDataDir </> "styles" </> path

-- | Run an PulpIO action. Recommended to call only once.
runPulpIO :: PulpArg -> PulpIO a -> IO a
runPulpIO PulpArg{..} (PulpIO act) = do
  logStderr $ \logger -> do
    let pulpLogger = withVerbosity loggerVerbosity $ withFormat loggerFormat logger
    pulpXHandling <- liftIO startXIO
    pulpDataDir <- getEnv dataDirEnv
    runReaderT act PulpEnv{..}
