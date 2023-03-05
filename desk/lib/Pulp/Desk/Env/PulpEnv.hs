module Pulp.Desk.Env.PulpEnv (
  PulpEnv,
  PulpIO,
  PulpArg (..),
  runPulpIO,
) where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Pulp.Desk.PulpPath
import Pulp.Desk.System.X11.XHandle
import Pulp.Desk.Utils.LogPrint

-- | Pulp environment.
data PulpEnv = MkPulpEnv
  { logger :: !LevelLogger
  , xHook :: !XHook
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
  askLog = PulpIO $ asks (\env -> env.logger)

instance MonadXHook PulpIO where
  askXHook :: PulpIO XHook
  askXHook = PulpIO $ asks (\env -> env.xHook)

-- | Run an PulpIO action. Recommended to call only once.
runPulpIO :: PulpArg -> PulpIO a -> IO a
runPulpIO MkPulpArg{..} (PulpIO act) = logStderr $ \inLogger -> do
  -- Path is set as global
  initDataDir argDataDir
  let logger = withVerbosity loggerVerbosity $ withFormat loggerFormat inLogger
  withXHook $ \xHook -> do
    runReaderT act MkPulpEnv{..}
