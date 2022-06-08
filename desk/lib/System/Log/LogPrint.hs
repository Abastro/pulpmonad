{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

-- | Thin logger facility on top of fast-logger, requires IO
module System.Log.LogPrint (
  module System.Log.FastLogger,
  LogSrc,
  LogLevel (..),
  logLevelName,
  LogFormat,
  defLogFormat,
  withFormat,
  withVerbosity,
  MonadLog (..),
  LevelLogger,
  logS,
  logStrf,
  LogStrfType,
  logStderr,
) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Char
import Data.Either
import Data.Text qualified as T
import Data.Vector qualified as V
import System.Log.FastLogger
import Data.Maybe
import Control.Monad

type LogSrc = T.Text
data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError
  deriving (Eq, Ord)

logLevelName :: LogLevel -> LogStr
logLevelName = \case
  LevelDebug -> toLogStr "DEBUG"
  LevelInfo -> toLogStr "INFO"
  LevelWarn -> toLogStr "WARN"
  LevelError -> toLogStr "ERROR"

type LogFormat = LogSrc -> LogLevel -> LogStr -> LogStr

-- | Default log format.
--
-- >>> defLogFormat (T.pack "MySource") LevelInfo (toLogStr "Did something happen?")
-- "[INFO#MySource] Did something happen?\n"
defLogFormat :: LogFormat
defLogFormat src level str = logStrPrinting "[$1$2] $3\n" [logLevelName level, srcSuffix, str]
  where
    srcSuffix = if T.null src then mempty else toLogStr "#" <> toLogStr src

type LevelLogger = LogSrc -> LogLevel -> FastLogger

-- | Logger with certain format. Use this to build MonadLog.
withFormat :: LogFormat -> FastLogger -> LevelLogger
withFormat format = \logger src level -> logger . format src level

-- | Control Verbosity so that it does not print so much.
withVerbosity :: LogLevel -> LevelLogger -> LevelLogger
withVerbosity cutoff logger = \src level str -> when (level >= cutoff) $ logger src level str

-- TODO Error level handling - verbosity?

class MonadIO m => MonadLog m where
  askLog :: m LevelLogger

logS :: MonadLog m => LogSrc -> LogLevel -> LogStr -> m ()
logS src level msg = askLog >>= \logger -> liftIO (logger src level msg)

-- Supports only '$1' style arguments
logStrPrinting :: String -> [LogStr] -> LogStr
logStrPrinting inp args = foldMap asLog $ gatherRights (divides inp)
  where
    divides = \case
      "" -> []
      '\\' : c : rem -> Right '\\' : Right c : divides rem
      '$' : str | (digits, rem) <- span isDigit str -> Left (read @Int digits) : divides rem
      c : rem -> Right c : divides rem
    gatherRights = \case
      [] -> []
      Left d : rem -> Left d : gatherRights rem
      ls | (rights, rem) <- span isRight ls -> sequenceA rights : gatherRights rem

    argVec = V.fromList args
    asLog = \case
      Left n -> fromMaybe (error "printf arg out of bounds") $ argVec V.!? pred n
      Right msg -> toLogStr msg

-- | Log string format.
-- No instance for MonadLog, since printf-style vararg is unwieldy for generic monads.
logStrf :: LogStrfType l => String -> l
logStrf format = intLogStrf format []

class LogStrfType l where
  intLogStrf :: String -> [LogStr] -> l

instance (ToLogStr a, LogStrfType l) => LogStrfType (a -> l) where
  intLogStrf format args = \arg -> intLogStrf format (toLogStr arg : args)

instance LogStrfType LogStr where
  intLogStrf format args = logStrPrinting format (reverse args)

-- | Creates a logger to stderr and runs the logging action on it.
logStderr :: (MonadUnliftIO m) => (FastLogger -> m a) -> m a
logStderr logs = withRunInIO $ \unlift -> withFastLogger (LogStderr defaultBufSize) (unlift . logs)
