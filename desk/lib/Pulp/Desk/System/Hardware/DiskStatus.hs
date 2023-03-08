{-# LANGUAGE OverloadedStrings #-}

module Pulp.Desk.System.Hardware.DiskStatus (diskSpeed, diskOf, diskStat) where

import Control.Applicative
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Generic.Data
import Pulp.Desk.Utils.ParseHor qualified as Parse
import System.FilePath

-- | Disk statistics for IO operations, i.e. Disk Read/Write.
data IOStat a = MkIOStat
  { numReqs :: !a
  , numMerges :: !a
  , numSectors :: !a
  , timeSpentms :: !a
  }
  deriving stock (Show, Generic1)
  deriving (Functor, Applicative) via (Generically1 IOStat)

-- | Disk speed in bit/s from IOStat.
diskSpeed :: (Real a, Fractional b) => IOStat a -> b
diskSpeed MkIOStat{..} =
  if timeSpentms == 0
    then 0 -- v MAYBE Query disk sector size instead of 4096
    else (4096 * 1000 * realToFrac numSectors) / realToFrac timeSpentms

-- | Disk statistics. Note that the units differ.
-- Consult <https://www.kernel.org/doc/html/latest/admin-guide/iostats.html>.
data DiskStat a = MkDiskStat
  { readStat :: !(IOStat a)
  , writeStat :: !(IOStat a)
  , ioMilliSecs :: !a
  -- ^ 10th field, not 9th
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
        Just MkDiskStat{..}
  _ -> Nothing
  where
    ioOf = \case
      numReqs : numMerges : numSectors : timeSpentms : _ -> Just $ MkIOStat{..}
      _ -> Nothing

-- | Gets disk statistics accumulated from booting.
-- Since multiple disks & disk partitions exist in many cases, map of non-loop disks are returned.
--
-- Pulls from </proc/diskstats>.
diskStat :: IO (M.Map T.Text (DiskStat Int))
diskStat = do
  Parse.parseFile disks ("/" </> "proc" </> "diskstats")
  where
    disks =
      Parse.fieldsIgnoring (Parse.skipH <* Parse.identH <* Parse.identH) (many Parse.decimalH)
        >>= Parse.exQueryMap query
    query = Parse.queryAllAs (not . ("loop" `T.isPrefixOf`)) (traverse diskOf)

-- Brightness: "/sys/class/backlight/?"
-- Network Load: "/sys/class/net/{device}" , "/proc/net/dev"
-- For detailed info, interfacing with NetworkManager should be better
