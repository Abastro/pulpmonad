{-# LANGUAGE OverloadedStrings #-}

module Pulp.Desk.System.Hardware.CPUStatus (
  cpuZero,
  cpuOf,
  cpuRatios,
  cpuUsed,
  cpuStat,
  cpuTemperature,
) where

import Control.Applicative
import Data.Map.Strict qualified as M
import Data.Monoid
import Data.Text qualified as T
import Generic.Data
import Pulp.Desk.System.Hardware.Commons
import Pulp.Desk.Utils.ParseHor qualified as Parse
import System.Directory
import System.FilePath

-- | CPU statistics. Usual unit is USER_HZ (typically 0.01s)
data CPUStat a = MkCPUStat
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

cpuZero :: Num a => CPUStat a
cpuZero = MkCPUStat{..}
  where
    userTime : niceTime : systemTime : idleTime : ioWait : irqTime : softirqTime : _ = repeat 0

cpuOf :: [a] -> Maybe (CPUStat a)
cpuOf = \case
  userTime : niceTime : systemTime : idleTime : ioWait : irqTime : softirqTime : _ -> Just MkCPUStat{..}
  _ -> Nothing

cpuRatios :: (Real a, Fractional b) => CPUStat a -> CPUStat b
cpuRatios cpu@MkCPUStat{userTime, systemTime, idleTime} = ratioTo (userTime + systemTime + idleTime) cpu

cpuUsed :: Num a => CPUStat a -> a
cpuUsed MkCPUStat{..} = userTime + systemTime

-- | Gets CPU statistics in accumulated from booting. Second of the pair is for each core.
-- Pulls from </proc/stat>.
cpuStat :: IO (CPUStat Int, [CPUStat Int])
cpuStat = Parse.parseFile cpus ("/" </> "proc" </> "stat")
  where
    cpus = Parse.fieldsCustom cpuFieldN (many Parse.decimalH) >>= Parse.exQueryMap query
    cpuFieldN = Parse.identCondH ("cpu" `T.isPrefixOf`)
    query = do
      total <- Parse.queryFieldAs "cpu" cpuOf
      cores <- Parse.queryAllAs ("cpu" `T.isPrefixOf`) (traverse cpuOf . M.elems)
      pure (total, cores)

-- | Gets CPU temperature, currently only handles k10temp. (MAYBE handle intel's coretemp)
--
-- Pulls from </sys/class/hwmon/hwmon?/temp1_input>.
--
-- Uses "Tctl" instead of "Tdie" as the latter is often not available.
cpuTemperature :: IO Double
cpuTemperature = do
  dirs <- map (baseDir </>) <$> listDirectory baseDir
  (* 0.001) <$> getAlt (foldMap (Alt . withName) dirs) <|> pure 0
  where
    baseDir = "/" </> "sys" </> "class" </> "hwmon"
    withName dir =
      readSingleLine (dir </> "name") >>= \case
        "k10temp" -> Parse.parseFile Parse.decimalH (dir </> "temp1_input")
        name -> fail $ "Not relevant device: " <> show name
