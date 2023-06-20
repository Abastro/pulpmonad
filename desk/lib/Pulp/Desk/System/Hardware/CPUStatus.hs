{-# LANGUAGE OverloadedStrings #-}

module Pulp.Desk.System.Hardware.CPUStatus (
  CPUStat (..),
  cpuZero,
  cpuOf,
  cpuRatios,
  cpuUsed,
  cpuStat,
  parseCPUStat,
  cpuTemperature,
) where

import Control.Applicative
import Data.List
import Data.Map.Strict qualified as M
import Data.Monoid
import Data.Text qualified as T
import Generic.Data
import Pulp.Desk.System.Hardware.Commons
import Pulp.Desk.Utils.ParseHor qualified as Parse
import System.Directory
import System.FilePath
import Text.Read

-- | CPU statistics. Usual unit is USER_HZ (typically 0.01s)
data CPUStat a = MkCPUStat
  { userTime :: !a
  , niceTime :: !a
  , systemTime :: !a
  , idleTime :: !a
  }
  deriving stock (Show, Eq, Generic1)
  deriving (Functor, Applicative) via (Generically1 CPUStat)

cpuZero :: Num a => CPUStat a
cpuZero = MkCPUStat{..}
  where
    userTime : niceTime : systemTime : idleTime : _ = repeat 0

cpuOf :: [a] -> Maybe (CPUStat a)
cpuOf = \case
  userTime : niceTime : systemTime : idleTime : _ -> Just MkCPUStat{..}
  _ -> Nothing

cpuRatios :: (Real a, Fractional b) => CPUStat a -> CPUStat b
cpuRatios cpu@MkCPUStat{userTime, systemTime, idleTime} = ratioTo (userTime + systemTime + idleTime) cpu

cpuUsed :: Num a => CPUStat a -> a
cpuUsed MkCPUStat{..} = userTime + systemTime

-- | Gets CPU statistics in accumulated from booting. Second of the pair is for each core.
-- Pulls from </proc/stat>.
cpuStat :: IO (CPUStat Int, [CPUStat Int])
cpuStat = Parse.parseFile parseCPUStat ("/" </> "proc" </> "stat")

parseCPUStat :: Parse.ParseHor (CPUStat Int, [CPUStat Int])
parseCPUStat = Parse.fields (many Parse.signedDecimalH) >>= Parse.exQueryMap query
  where
    query = do
      total <- Parse.queryFieldAs "cpu" cpuOf
      cpus <- Parse.queryAllAs isSpecificCPU (fmap sortUsingFst . traverse formatted . M.toList)
      pure (total, cpus)

    sortUsingFst = fmap @[] snd . sortOn fst

    -- We assume when a field name starts with "cpu", it is about a cpu.
    isSpecificCPU name = case T.stripPrefix "cpu" name of
      Just num -> not (T.null num)
      Nothing -> False

    formatted (name, val) = (,) <$> readMaybe @Int (T.unpack $ T.drop 3 name) <*> cpuOf val

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
