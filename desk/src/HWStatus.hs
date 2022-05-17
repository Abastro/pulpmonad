{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HWStatus
  ( CPUStat (..),
    cpuUsed,
    cpuUsedRatio,
    cpuStat,
    cpuDiff,
    cpuTemp,
    MemStat (..),
    memUsed,
    memUsedRatio,
    memStat,
    BatStatus (..),
    BatStat (..),
    batStat,
  )
where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Identity
import Data.Char
import Data.List (isPrefixOf)
import Data.Map qualified as M
import Data.Monoid
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void
import System.Directory
import System.FilePath
import System.IO
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex

-- | Gets a single line from a file
getFileLine :: FilePath -> IO T.Text
getFileLine path = withFile path ReadMode T.hGetLine

parseFile :: P.Parsec Void T.Text a -> FilePath -> IO a
parseFile parser path =
  P.parse parser path <$> T.readFile path >>= \case
    Left err -> fail $ P.errorBundlePretty err
    Right result -> pure result

-- | Skips horizontally.
skipH :: P.MonadParsec e T.Text m => m ()
skipH = Lex.space P.hspace1 empty empty

-- | Parses remaining characters until a line break.
remainH :: P.MonadParsec e T.Text m => m T.Text
remainH = P.takeWhileP (Just "remaining") $ (/= '\n')

-- | Space-separated identifier, which could include '(' and ')'.
identH :: P.MonadParsec e T.Text m => m T.Text
identH = Lex.lexeme skipH (P.takeWhileP (Just "identifier") isID)
  where
    isID c = isAlphaNum c || c == '(' || c == ')' || c == '_'

-- | Horizontal symbols.
symbolH :: P.MonadParsec e T.Text m => T.Text -> m T.Text
symbolH = Lex.symbol skipH

-- | Horizontal decimals.
decimalH :: (P.MonadParsec e T.Text m, Num a) => m a
decimalH = Lex.lexeme skipH Lex.decimal

-- | Parse an end of line, or eof.
eoH :: P.MonadParsec e T.Text m => m ()
eoH = () <$ P.eol <|> P.eof

-- | CPU statistics. Unit is USER_HZ (?)
data CPUStat = CPUStat
  { userTime :: Int,
    niceTime :: Int,
    systemTime :: Int,
    idleTime :: Int,
    ioWait :: Int,
    irqTime :: Int,
    softirqTime :: Int
  }
  deriving (Show)

cpuUsed :: CPUStat -> Int
cpuUsed CPUStat {..} = userTime + systemTime

cpuUsedRatio :: CPUStat -> Double
cpuUsedRatio cpu@CPUStat {idleTime} = (fromIntegral $ cpuUsed cpu) / (fromIntegral $ cpuUsed cpu + idleTime)

-- | Gets CPU statistics in accumulative sense. Second of the pair is for each core.
-- Pulls from "/proc/stat".
cpuStat :: IO (CPUStat, [CPUStat])
cpuStat = parseFile cpus ("/" </> "proc" </> "stat")
  where
    cpus = (,) <$> cpu <*> many cpu -- NOTE: We only parse the beginning.
    cpu = do
      P.try $ identH >>= guard . ("cpu" `T.isPrefixOf`)
      userTime : niceTime : systemTime : idleTime : ioWait : irqTime : softirqTime : _ <-
        many decimalH <* eoH
      pure CPUStat {..}

-- | Compute time spent in each mode during specified amount of time (ms).
cpuDiff :: Int -> IO CPUStat
cpuDiff delay = do
  CPUStat t1 t2 t3 t4 t5 t6 t7 <- fst <$> cpuStat
  threadDelay (delay * 1000)
  CPUStat t1' t2' t3' t4' t5' t6' t7' <- fst <$> cpuStat
  pure $ CPUStat (t1' - t1) (t2' - t2) (t3' - t3) (t4' - t4) (t5' - t5) (t6' - t6) (t7' - t7)

-- | Gets CPU temperature, currently only handles k10temp. (MAYBE handle intel's coretemp)
-- Pulls from "/sys/class/hwmon/hwmon?/temp2_input".
cpuTemp :: IO Double
cpuTemp = do
  dirs <- map (baseDir </>) <$> listDirectory baseDir
  (* 0.001) <$> getAlt (foldMap (Alt . withName) dirs) <|> pure 0
  where
    baseDir = "/" </> "sys" </> "class" </> "hwmon"
    withName dir =
      getFileLine (dir </> "name") >>= \case
        "k10temp" -> parseFile Lex.decimal (dir </> "temp2_input")
        name -> fail $ "Not relevant device: " <> show name

-- | Memory statistics. All units are in kB.
data MemStat = MemStat
  { memTotal :: Int,
    memFree :: Int,
    memAvailable :: Int,
    memBuffers :: Int,
    memCached :: Int,
    swapTotal :: Int,
    swapFree :: Int
  }
  deriving (Show)

memUsed :: MemStat -> Int
memUsed MemStat {..} = memTotal - memFree - memBuffers - memCached

memUsedRatio :: MemStat -> Double
memUsedRatio mem@MemStat {memTotal} = (fromIntegral $ memUsed mem) / fromIntegral memTotal

-- | Gets Memory statistics.
-- Pulls from "/proc/meminfo".
memStat :: IO MemStat
memStat = parseFile memory ("/" </> "proc" </> "meminfo")
  where
    memory = do
      mmap <- M.fromList <$> many memLine
      case traverse (mmap M.!?) ["MemTotal", "MemFree", "MemAvailable", "Buffers", "Cached", "SwapTotal", "SwapFree"] of
        Just [memTotal, memFree, memAvailable, memBuffers, memCached, swapTotal, swapFree] ->
          pure $ MemStat {..}
        _ -> fail $ "Ill-formed map parsed: " <> show mmap
    memLine = (,) <$> identH <*> (symbolH ":" *> mayKB <* eoH)
    mayKB = decimalH <* P.optional (symbolH "kB")

-- Note: NotCharging = "Not Charging".
data BatStatus = Charging | Discharging | NotCharging | Full | Unknown
  deriving (Show)

-- | Battery statistics. Some components may or may not exist.
-- Units are given as follows:
--
-- * capacity: %
-- * energy: μWh
-- * charge: μAh
-- * voltage: μV
-- * power: μW
-- * current: μA
data BatStat = BatStat
  { batStatus :: BatStatus,
    -- | Current capacity in percentage to full
    capacity :: Int,
    energyFull :: Maybe Int,
    chargeFull :: Maybe Int,
    energyFullDesign :: Maybe Int,
    chargeFullDesign :: Maybe Int,
    energyNow :: Maybe Int,
    chargeNow :: Maybe Int,
    voltageNow :: Maybe Int,
    voltageMinDesign :: Maybe Int,
    currentNow :: Maybe Int,
    powerNow :: Maybe Int
  }
  deriving (Show)

-- | Gets Battery statistics.
-- Pulls from " /sys/class/power_supply/BAT?/uevent".
batStat :: IO BatStat
batStat = do
  let path = "/" </> "sys" </> "class" </> "power_supply"
  batName : _ <- filter ("BAT" `isPrefixOf`) <$> listDirectory path
  parseFile battery (path </> batName </> "uevent")
  where
    battery = do
      bmap <- M.fromList <$> many batLine
      (status, capacity) <-
        case (,) <$> bmap M.!? "POWER_SUPPLY_STATUS" <*> bmap M.!? "POWER_SUPPLY_CAPACITY" of
          Just (Left status, Right capacity) -> pure (status, capacity)
          _ -> fail $ "Cannot find status and/or capacity from: " <> show bmap
      let batStatus = statusEnum status
          intField str = bmap M.!? str >>= either (const Nothing) Just
          energyFull = intField "POWER_SUPPLY_ENERGY_FULL"
          chargeFull = intField "POWER_SUPPLY_CHARGE_FULL"
          energyFullDesign = intField "POWER_SUPPLY_ENERGY_FULL_DESIGN"
          chargeFullDesign = intField "POWER_SUPPLY_CHARGE_FULL_DESIGN"
          energyNow = intField "POWER_SUPPLY_ENERGY_NOW"
          chargeNow = intField "POWER_SUPPLY_CHARGE_NOW"
          voltageNow = intField "POWER_SUPPLY_VOLTAGE_NOW"
          voltageMinDesign = intField "POWER_SUPPLY_VOLTAGE_MIN_DESIGN"
          currentNow = intField "POWER_SUPPLY_CURRENT_NOW"
          powerNow = intField "POWER_SUPPLY_POWER_NOW"
      pure $ BatStat {..}

    batLine = (,) <$> identH <*> (symbolH "=" *> decOrStr <* eoH)
    decOrStr = Right <$> decimalH <|> Left <$> remainH
    statusEnum = \case
      "Charging" -> Charging
      "Discharging" -> Discharging
      "Not charging" -> NotCharging
      "Full" -> Full
      _ -> Unknown

-- Disk is complex, perhaps /etc/mtab & /proc/diskstats

-- Brightness: "/sys/class/backlight/?"
