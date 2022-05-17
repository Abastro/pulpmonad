{-# LANGUAGE OverloadedStrings #-}

module HWStatus (
  CPUStat (..),
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
  DiskStat (..),
  diskStat,
) where

import Control.Applicative
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

-- | CPU statistics. Unit is USER_HZ (typically 0.01s)
data CPUStat = CPUStat
  { userTime :: Int
  , niceTime :: Int
  , systemTime :: Int
  , idleTime :: Int
  , ioWait :: Int
  , irqTime :: Int
  , softirqTime :: Int
  }
  deriving (Show)

cpuOf :: [Int] -> Maybe CPUStat
cpuOf = \case
  userTime : niceTime : systemTime : idleTime : ioWait : irqTime : softirqTime : _ -> Just CPUStat{..}
  _ -> Nothing

cpuUsed :: CPUStat -> Int
cpuUsed CPUStat{..} = userTime + systemTime

cpuUsedRatio :: CPUStat -> Double
cpuUsedRatio cpu@CPUStat{idleTime} = (fromIntegral $ cpuUsed cpu) / (fromIntegral $ cpuUsed cpu + idleTime)

{- |
  Gets CPU statistics in accumulative sense. Second of the pair is for each core.
  Pulls from </proc/stat>.
-}
cpuStat :: IO (CPUStat, [CPUStat])
cpuStat = parseFile cpus ("/" </> "proc" </> "stat")
  where
    cpus = fields (many decimalH) >>= exQueryMap query
    query = do
      total <- queryFieldAs "cpu" cpuOf
      cores <- queryAllAs ("cpu" `T.isPrefixOf`) (traverse cpuOf . M.elems)
      pure (total, cores)

{- |
  Compute time spent in each mode during specified amount of time (ms).
  Argument should be positive.
-}
cpuDiff :: Int -> IO CPUStat
cpuDiff delay = do
  CPUStat t1 t2 t3 t4 t5 t6 t7 <- fst <$> cpuStat
  threadDelay (delay * 1000)
  CPUStat t1' t2' t3' t4' t5' t6' t7' <- fst <$> cpuStat
  pure $ CPUStat (t1' - t1) (t2' - t2) (t3' - t3) (t4' - t4) (t5' - t5) (t6' - t6) (t7' - t7)

{- |
  Gets CPU temperature, currently only handles k10temp. (MAYBE handle intel's coretemp)
  Pulls from </sys/class/hwmon/hwmon?/temp2_input>.
-}
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
  { memTotal :: Int
  , memFree :: Int
  , memAvailable :: Int
  , memBuffers :: Int
  , memCached :: Int
  , swapTotal :: Int
  , swapFree :: Int
  }
  deriving (Show)

memUsed :: MemStat -> Int
memUsed MemStat{..} = memTotal - memFree - memBuffers - memCached

memUsedRatio :: MemStat -> Double
memUsedRatio mem@MemStat{memTotal} = (fromIntegral $ memUsed mem) / fromIntegral memTotal

{- |
  Gets Memory statistics.
  Pulls from </proc/meminfo>.
-}
memStat :: IO MemStat
memStat = parseFile memory ("/" </> "proc" </> "meminfo")
  where
    memory = do
      fields (symbolH ":" *> mayKB) >>= exQueryMap query

    mayKB = label "data" $ decimalH <* optional (symbolH "kB")
    query = do
      memTotal <- queryField "MemTotal"
      memFree <- queryField "MemFree"
      memAvailable <- queryField "MemAvailable"
      memBuffers <- queryField "Buffers"
      memCached <- queryField "Cached"
      swapTotal <- queryField "SwapTotal"
      swapFree <- queryField "SwapFree"
      pure MemStat{..}

-- Note: NotCharging = "Not Charging".
data BatStatus = Charging | Discharging | NotCharging | Full | Unknown
  deriving (Show)

{- |
  Battery statistics. Some components may or may not exist.
  Units are given as follows:

  * capacity: %
  * energy: μWh
  * charge: μAh
  * voltage: μV
  * power: μW
  * current: μA
-}
data BatStat = BatStat
  { batStatus :: BatStatus
  , capacity :: Int
  -- ^ Current capacity in percentage to full
  , energyFull :: Maybe Int
  , chargeFull :: Maybe Int
  , energyFullDesign :: Maybe Int
  , chargeFullDesign :: Maybe Int
  , energyNow :: Maybe Int
  , chargeNow :: Maybe Int
  , voltageNow :: Maybe Int
  , voltageMinDesign :: Maybe Int
  , currentNow :: Maybe Int
  , powerNow :: Maybe Int
  }
  deriving (Show)

{- |
  Gets Battery statistics.
  Pulls from </sys/class/power_supply/BAT?/uevent>.
-}
batStat :: IO BatStat
batStat = do
  let path = "/" </> "sys" </> "class" </> "power_supply"
  batName : _ <- filter ("BAT" `isPrefixOf`) <$> listDirectory path
  parseFile battery (path </> batName </> "uevent")
  where
    battery = fields (symbolH "=" *> decOrStr) >>= exQueryMap query
    decOrStr = label "data" $ Right <$> decimalH <|> Left <$> remainH

    query = do
      let asStr = either Just (const Nothing)
          asInt = either (const Nothing) Just
      batStatus <- queryFieldAs "POWER_SUPPLY_STATUS" (fmap statusEnum . asStr)
      capacity <- queryFieldAs "POWER_SUPPLY_CAPACITY" asInt
      energyFull <- queryOptAs "POWER_SUPPLY_ENERGY_FULL" asInt
      chargeFull <- queryOptAs "POWER_SUPPLY_CHARGE_FULL" asInt
      energyFullDesign <- queryOptAs "POWER_SUPPLY_ENERGY_FULL_DESIGN" asInt
      chargeFullDesign <- queryOptAs "POWER_SUPPLY_CHARGE_FULL_DESIGN" asInt
      energyNow <- queryOptAs "POWER_SUPPLY_ENERGY_NOW" asInt
      chargeNow <- queryOptAs "POWER_SUPPLY_CHARGE_NOW" asInt
      voltageNow <- queryOptAs "POWER_SUPPLY_VOLTAGE_NOW" asInt
      voltageMinDesign <- queryOptAs "POWER_SUPPLY_VOLTAGE_MIN_DESIGN" asInt
      currentNow <- queryOptAs "POWER_SUPPLY_CURRENT_NOW" asInt
      powerNow <- queryOptAs "POWER_SUPPLY_POWER_NOW" asInt
      pure BatStat{..}
    statusEnum = \case
      "Charging" -> Charging
      "Discharging" -> Discharging
      "Not charging" -> NotCharging
      "Full" -> Full
      _ -> Unknown

{- |
  Disk statistics.
  Consult <https://www.kernel.org/doc/html/latest/admin-guide/iostats.html>.
-}
data DiskStat = DiskStat
  { numReads :: Int
  , readMerged :: Int
  , readSectors :: Int
  , readMilliSecs :: Int
  , numWrites :: Int
  , writeMerged :: Int
  , writeSectors :: Int
  , writeMilliSecs :: Int
  , ioMilliSecs :: Int
  -- ^ 10th field, not 9th
  } deriving (Show)

diskOf :: [Int] -> Maybe DiskStat
diskOf = \case
  numReads : readMerged : readSectors : readMilliSecs : numWrites : writeMerged : writeSectors : writeMilliSecs : _ : ioMilliSecs : _ ->
    Just DiskStat{..}
  _ -> Nothing

{- |
  Gets disk statistics.
  Since multiple disks & disk partitions exist in many cases, map of non-loop disks are returned.

  Pulls from </proc/diskstats>.
-}
diskStat :: IO (M.Map T.Text DiskStat)
diskStat = do
  parseFile disks ("/" </> "proc" </> "diskstats")
  where
    disks = fieldsWithHead (skipH <* identH <* identH) (many decimalH) >>= exQueryMap query
    query = queryAllAs (not . ("loop" `T.isPrefixOf`)) (traverse diskOf)

-- TODO Implement disk diff

-- Brightness: "/sys/class/backlight/?"
