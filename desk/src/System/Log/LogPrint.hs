{-# LANGUAGE GADTs #-}

-- | Thin logger facility on top of fast-logger, requires IO
module System.Log.LogPrint (
  module System.Log.FastLogger,
  LogSrc,
  LogLevel (..),
  logLevelName,
  defLogFormat,
  withFM,
  MonadLog (..),
  logPrintf,
  LogPrintfType,
  logStderr,
) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Char
import Data.Either
import Data.Text qualified as T
import Data.Vector qualified as V
import System.Log.FastLogger

type LogSrc = T.Text
data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError
  deriving (Eq, Ord)

logLevelName :: LogLevel -> LogStr
logLevelName = \case
  LevelDebug -> toLogStr "DEBUG"
  LevelInfo -> toLogStr "INFO"
  LevelWarn -> toLogStr "WARN"
  LevelError -> toLogStr "ERROR"

-- | Default log format.
--
-- >>> defLogFormat (T.pack "MySource") LevelInfo (toLogStr "Did something happen?")
-- "[INFO#MySource] Did something happen?\n"
defLogFormat :: LogSrc -> LogLevel -> LogStr -> LogStr
defLogFormat src level str = logStrPrinting "[$1$2] $3\n" [logLevelName level, srcSuffix, str]
  where
    srcSuffix = case T.null src of
      True -> mempty
      False -> toLogStr "#" <> toLogStr src

-- TODO Error level handling - verbosity?

-- | Logger with certain format. Use this to build MonadLog.
withFM :: Functor m => (LogSrc -> LogLevel -> LogStr -> LogStr) -> (m FastLogger -> LogSrc -> LogLevel -> m FastLogger)
withFM format baseLog src level = fmap (. format src level) baseLog

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
      Left n -> argVec V.! pred n
      Right msg -> toLogStr msg

class MonadIO m => MonadLog m where
  askLog :: LogSrc -> LogLevel -> m FastLogger

-- | Printf for loggers.
--
-- >>> (logPrintf "[$1]($3): $2 -> $4") 1 2 3 4 :: LogStr
-- "[1](3): 2 -> 4"
logPrintf :: LogPrintfType l => String -> l
logPrintf inp = inLogPrintf inp []

class LogPrintfType l where
  inLogPrintf :: String -> [LogStr] -> l

-- In need of reverse
instance (ToLogStr a, LogPrintfType l) => LogPrintfType (a -> l) where
  inLogPrintf inp args = \arg -> inLogPrintf inp (toLogStr arg : args)

instance LogPrintfType LogStr where
  inLogPrintf inp args = logStrPrinting inp (reverse args)

instance (a ~ (), MonadIO m) => LogPrintfType (FastLogger -> m ()) where
  inLogPrintf inp args = \logger -> liftIO . logger $ inLogPrintf inp args

-- | Creates a logger to stderr and runs the logging action on it.
logStderr :: (MonadUnliftIO m) => (FastLogger -> m a) -> m a
logStderr logs = withRunInIO $ \unlift -> withFastLogger (LogStderr defaultBufSize) (unlift . logs)
