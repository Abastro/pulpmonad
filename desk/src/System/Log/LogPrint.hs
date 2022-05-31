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
) where

import Control.Monad.IO.Class
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
  LevelDebug -> toLogStr "Debug"
  LevelInfo -> toLogStr "Info"
  LevelWarn -> toLogStr "Warn"
  LevelError -> toLogStr "Error"

-- | Default log format.
--
-- >>> defLogFormat (T.pack "MySource") LevelInfo (toLogStr "Did something happen?")
-- "[Info#MySource] Did something happen?\n"
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

logPrintf :: LogPrintfType l => FastLogger -> String -> l
logPrintf logger inp = inLogPrintf logger inp []

class LogPrintfType l where
  inLogPrintf :: FastLogger -> String -> [LogStr] -> l

-- In need of reverse
instance (ToLogStr a, LogPrintfType l) => LogPrintfType (a -> l) where
  inLogPrintf logger inp args = \arg -> inLogPrintf logger inp (toLogStr arg : args)

instance (a ~ (), MonadIO m) => LogPrintfType (m ()) where
  inLogPrintf logger inp args = liftIO . logger $ logStrPrinting inp (reverse args)
