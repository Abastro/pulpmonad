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
import Control.Applicative.Combinators
import Control.Concurrent
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as M
import Data.Monoid
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Parse.ParseHor
import System.Directory
import System.FilePath
import System.IO

-- | Gets a single line from a file
getFileLine :: FilePath -> IO T.Text
getFileLine path = withFile path ReadMode T.hGetLine

procStat :: IO (M.Map T.Text [Int])
procStat = parseFile stat ("/" </> "proc" </> "stat")
  where
    stat = M.fromList <$> statLine `sepEndBy` eoH
    statLine = label "stat-line" $ (,) <$> identH <*> many decimalH

-- | CPU statistics. Unit is USER_HZ (typically 0.01s)
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

cpuOf :: [Int] -> Maybe CPUStat
cpuOf = \case
  userTime : niceTime : systemTime : idleTime : ioWait : irqTime : softirqTime : _ -> Just $ CPUStat {..}
  _ -> Nothing

cpuUsed :: CPUStat -> Int
cpuUsed CPUStat {..} = userTime + systemTime

cpuUsedRatio :: CPUStat -> Double
cpuUsedRatio cpu@CPUStat {idleTime} = (fromIntegral $ cpuUsed cpu) / (fromIntegral $ cpuUsed cpu + idleTime)

-- | Gets CPU statistics in accumulative sense. Second of the pair is for each core.
-- Pulls from "/proc/stat".
cpuStat :: IO (CPUStat, [CPUStat])
cpuStat = do
  smap <- procStat
  total <- maybe (fail "/proc/stat, cpu(total) ill-formed") pure $ smap M.!? "cpu" >>= cpuOf
  cores <-
    maybe (fail "/proc/stat, cpu cores ill-formed") pure $
      traverse cpuOf . M.elems $ M.filterWithKey (\k _ -> "cpu" /= k && "cpu" `T.isPrefixOf` k) smap
  pure (total, cores)

-- | Compute time spent in each mode during specified amount of time (ms).
-- Argument should be positive.
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
        "k10temp" -> parseFile decimalH (dir </> "temp2_input")
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
      mmap <- M.fromList <$> memLine `sepEndBy` eoH
      case traverse (mmap M.!?) ["MemTotal", "MemFree", "MemAvailable", "Buffers", "Cached", "SwapTotal", "SwapFree"] of
        Just [memTotal, memFree, memAvailable, memBuffers, memCached, swapTotal, swapFree] ->
          pure $ MemStat {..}
        _ -> fail $ "Ill-formed map parsed: " <> show mmap
    memLine = label "meminfo-line" $ (,) <$> identH <*> (symbolH ":" *> mayKB)
    mayKB = label "field" $ decimalH <* optional (symbolH "kB")

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
      bmap <- M.fromList <$> batLine `sepEndBy` eoH
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

    batLine = label "battery-uevent-line" $ (,) <$> identH <*> (symbolH "=" *> decOrStr)
    decOrStr = label "field" $ Right <$> decimalH <|> Left <$> remainH
    statusEnum = \case
      "Charging" -> Charging
      "Discharging" -> Discharging
      "Not charging" -> NotCharging
      "Full" -> Full
      _ -> Unknown

-- Disk is complex, perhaps /etc/mtab & /proc/diskstats
-- Actually I could just.. let the user select.

-- Brightness: "/sys/class/backlight/?"
