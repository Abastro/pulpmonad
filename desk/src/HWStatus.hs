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
import Data.List (isPrefixOf)
import Data.Map qualified as M
import Data.Monoid
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory
import System.FilePath
import System.IO
import Text.Parsec qualified as P
import Text.Parsec.Text qualified as P
import Text.Parsec.Token qualified as P

-- | Gets a single line from a file
getFileLine :: FilePath -> IO T.Text
getFileLine path = withFile path ReadMode T.hGetLine

simpleLex :: P.GenTokenParser T.Text () Identity
simpleLex = P.makeTokenParser simpleLangDef
  where
    simpleLangDef =
      P.LanguageDef
        { P.commentStart = "/*",
          P.commentEnd = "*/",
          P.commentLine = "#",
          P.nestedComments = True,
          P.identStart = P.letter,
          P.identLetter = P.alphaNum <|> P.oneOf "_'()",
          P.opStart = P.opLetter simpleLangDef,
          P.opLetter = P.oneOf "=:",
          P.reservedNames = [],
          P.reservedOpNames = ["=", ":"],
          P.caseSensitive = True
        }

eol :: P.Parser ()
eol = () <$ P.newline <|> P.eof

skipSpaces :: P.Parser ()
skipSpaces = P.skipMany (P.char ' ')

-- | Natural number parsing, parses the whitespace
naturalNum :: Num a => P.Parser a
naturalNum = fromIntegral <$> P.natural simpleLex

-- | Decimal parsing, without parsing the whitespace
decimalNum :: Num a => P.Parser a
decimalNum = fromIntegral <$> P.decimal simpleLex

identStr :: P.Parser String
identStr = P.identifier simpleLex

symbolStr :: String -> P.Parser String
symbolStr str = P.symbol simpleLex str

operator :: String -> P.Parser ()
operator = P.reservedOp simpleLex

finalParse :: P.Parser a -> P.Parser a
finalParse parse = P.whiteSpace simpleLex *> parse <* P.eof

parseFile :: P.Parser a -> FilePath -> IO a
parseFile parser path =
  P.parseFromFile parser path >>= \case
    Left err -> fail $ show err
    Right result -> pure result

-- | CPU statistics. Unit is USER_HZ (?)
data CPUStat = CPUStat
  { userTime :: Int,
    niceTime :: Int,
    systemTime :: Int,
    idleTime :: Int,
    ioWait :: Int,
    irqTime :: Int,
    softirqTime :: Int
  } deriving Show

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
      P.try $ identStr >>= guard . ("cpu" `isPrefixOf`)
      userTime : niceTime : systemTime : idleTime : ioWait : irqTime : softirqTime : _ <-
        P.sepBy decimalNum skipSpaces <* eol
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
        "k10temp" -> parseFile (finalParse naturalNum) (dir </> "temp2_input")
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
      Just [memTotal, memFree, memAvailable, memBuffers, memCached, swapTotal, swapFree] <-
        pure $ traverse (mmap M.!?) ["MemTotal", "MemFree", "MemAvailable", "Buffers", "Cached", "SwapTotal", "SwapFree"]
      pure $ MemStat {..}
    memLine = (,) <$> identStr <*> (operator ":" *> mayKB)
    mayKB = naturalNum <* P.optional (symbolStr "kB")

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
      Just (Left status, Right capacity) <-
        pure $ (,) <$> bmap M.!? "POWER_SUPPLY_STATUS" <*> bmap M.!? "POWER_SUPPLY_CAPACITY"
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

    batLine = (,) <$> identStr <*> (operator "=" *> decOrStr)
    decOrStr = Right <$> naturalNum <|> Left <$> (P.lexeme simpleLex $ many (P.noneOf "\n"))
    statusEnum = \case
      "Charging" -> Charging
      "Discharging" -> Discharging
      "Not charging" -> NotCharging
      "Full" -> Full
      _ -> Unknown

-- Disk is complex, perhaps /etc/mtab & /proc/diskstats

-- Brightness: "/sys/class/backlight/?"
