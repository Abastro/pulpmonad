{-# LANGUAGE OverloadedStrings #-}

module Status.HWStatus (
  CPUStat (..),
  cpuRatios,
  cpuUsed,
  cpuStat,
  cpuDelta,
  cpuTemp,
  MemStat (..),
  memRatios,
  memUsed,
  memStat,
  BatStatus (..),
  BatStat (..),
  batStat,
  IOStat (..),
  DiskStat (..),
  diskSpeed,
  diskStat,
  diskDelta,
) where

import Control.Applicative
import Control.Concurrent
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as M
import Data.Monoid
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Generic.Data
import Parse.ParseHor
import System.Directory
import System.FilePath
import System.IO

-- | Gets a single line from a file.
getFileLine :: FilePath -> IO T.Text
getFileLine path = withFile path ReadMode T.hGetLine

-- | Ratio to certain value
ratioTo :: (Applicative v, Real a, Fractional b) => a -> v a -> v b
ratioTo un = fmap (\n -> realToFrac n / realToFrac un)

-- | CPU statistics. Usual unit is USER_HZ (typically 0.01s)
data CPUStat a = CPUStat
  { userTime :: !a
  , niceTime :: !a
  , systemTime :: !a
  , idleTime :: !a
  , ioWait :: !a
  , irqTime :: !a
  , softirqTime :: !a
  }
  deriving stock (Show, Generic1)
  deriving (Functor, Applicative) via (Generically1 CPUStat)

cpuOf :: [a] -> Maybe (CPUStat a)
cpuOf = \case
  userTime : niceTime : systemTime : idleTime : ioWait : irqTime : softirqTime : _ -> Just CPUStat{..}
  _ -> Nothing

cpuRatios :: (Real a, Fractional b) => CPUStat a -> CPUStat b
cpuRatios cpu@CPUStat{userTime, systemTime, idleTime} = ratioTo (userTime + systemTime + idleTime) cpu

cpuUsed :: Num a => CPUStat a -> a
cpuUsed CPUStat{..} = userTime + systemTime

-- | Gets CPU statistics in accumulated from booting. Second of the pair is for each core.
-- Pulls from </proc/stat>.
cpuStat :: IO (CPUStat Int, [CPUStat Int])
cpuStat = parseFile cpus ("/" </> "proc" </> "stat")
  where
    cpus = fieldsCustom cpuFieldN (many decimalH) >>= exQueryMap query
    cpuFieldN = identCondH ("cpu" `T.isPrefixOf`)
    query = do
      total <- queryFieldAs "cpu" cpuOf
      cores <- queryAllAs ("cpu" `T.isPrefixOf`) (traverse cpuOf . M.elems)
      pure (total, cores)

-- | CPU time spent in each mode during specified amount of time (ms).
-- The argument should be positive.
cpuDelta :: Int -> IO (CPUStat Int)
cpuDelta delay = do
  pre <- fst <$> cpuStat
  threadDelay (delay * 1000)
  post <- fst <$> cpuStat
  pure (liftA2 (-) post pre)

-- | Gets CPU temperature, currently only handles k10temp. (MAYBE handle intel's coretemp)
-- Pulls from </sys/class/hwmon/hwmon?/temp1_input>.
--
-- Changed to use "Tctl" instead of "Tdie" as the latter is often not available.
--
-- (Some CPUs adds offset to attain Tctl,
-- and frankly the adjusted temp is likely appropriate for display)
cpuTemp :: IO Double
cpuTemp = do
  dirs <- map (baseDir </>) <$> listDirectory baseDir
  (* 0.001) <$> getAlt (foldMap (Alt . withName) dirs) <|> pure 0
  where
    baseDir = "/" </> "sys" </> "class" </> "hwmon"
    withName dir =
      getFileLine (dir </> "name") >>= \case
        "k10temp" -> parseFile decimalH (dir </> "temp1_input")
        name -> fail $ "Not relevant device: " <> show name

-- | Memory statistics. Usual unit is kB.
data MemStat a = MemStat
  { memTotal :: !a
  , memFree :: !a
  , memAvailable :: !a
  , memBuffers :: !a
  , memCached :: !a
  , swapTotal :: !a
  , swapFree :: !a
  }
  deriving stock (Show, Generic1)
  deriving (Functor, Applicative) via (Generically1 MemStat)

memRatios :: (Real a, Fractional b) => MemStat a -> MemStat b
memRatios mem@MemStat{memTotal} = ratioTo memTotal mem

memUsed :: Num a => MemStat a -> a
memUsed MemStat{..} = memTotal - memFree - memBuffers - memCached

-- | Gets Memory statistics.
-- Pulls from </proc/meminfo>.
memStat :: IO (MemStat Int)
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

-- | Battery status, note that @NotCharging@ means "Not charging".
data BatStatus = Charging | Discharging | NotCharging | Full | Unknown
  deriving (Show)

-- |
--  Battery statistics. Some components may or may not exist.
--  Units are given as follows:
--
--  * capacity: %
--  * energy: μWh
--  * charge: μAh
--  * voltage: μV
--  * power: μW
--  * current: μA
data BatStat = BatStat
  { batStatus :: BatStatus
  , -- | Current capacity in percentage to full
    capacity :: Int
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

-- | Gets Battery statistics.
-- Pulls from </sys/class/power_supply/BAT?/uevent>.
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

-- | Disk statistics for either of Read/Write.
data IOStat a = IOStat
  { numReqs :: !a
  , numMerges :: !a
  , numSectors :: !a
  , timeSpentms :: !a
  }
  deriving stock (Show, Generic1)
  deriving (Functor, Applicative) via (Generically1 IOStat)

-- | Disk speed in bit/s from IOStat.
diskSpeed :: (Real a, Fractional b) => IOStat a -> b
diskSpeed IOStat{..} =
  if timeSpentms == 0
    then 0 -- v MAYBE Use disk sector size instead of this
    else (4096 * 1000 * realToFrac numSectors) / realToFrac timeSpentms

-- | Disk statistics. Note that the units differ.
-- Consult <https://www.kernel.org/doc/html/latest/admin-guide/iostats.html>.
data DiskStat a = DiskStat
  { readStat :: !(IOStat a)
  , writeStat :: !(IOStat a)
  , -- | 10th field, not 9th
    ioMilliSecs :: !a
  }
  deriving stock (Show, Generic1)
  deriving (Functor, Applicative) via (Generically1 DiskStat)

diskOf :: [a] -> Maybe (DiskStat a)
diskOf = \case
  xs
    | (reads, xs') <- splitAt 4 xs
      , (writes, ioMilliSecs : _) <- splitAt 4 xs'
      , Just readStat <- ioOf reads
      , Just writeStat <- ioOf writes ->
      Just DiskStat{..}
  _ -> Nothing
  where
    ioOf = \case
      numReqs : numMerges : numSectors : timeSpentms : _ -> Just $ IOStat{..}
      _ -> Nothing

-- | Gets disk statistics accumulated from booting.
-- Since multiple disks & disk partitions exist in many cases, map of non-loop disks are returned.
--
-- Pulls from </proc/diskstats>.
diskStat :: IO (M.Map T.Text (DiskStat Int))
diskStat = do
  parseFile disks ("/" </> "proc" </> "diskstats")
  where
    disks = fieldsWithHead (skipH <* identH <* identH) (many decimalH) >>= exQueryMap query
    query = queryAllAs (not . ("loop" `T.isPrefixOf`)) (traverse diskOf)

-- | Disk statistics as rates over certain delay(ms), for certain disk/partition.
-- The disk/partition name is analogous to the one from "df" command.
diskDelta :: Int -> IO (M.Map T.Text (DiskStat Int))
diskDelta delay = do
  pre <- diskStat
  threadDelay (delay * 1000)
  post <- diskStat
  let diff = M.intersectionWith (liftA2 (-)) pre post
  pure diff

-- Brightness: "/sys/class/backlight/?"
-- Network Load: "/sys/class/net/{device}" , "/proc/net/dev"
-- For detailed info, interfacing with NetworkManager should be better
