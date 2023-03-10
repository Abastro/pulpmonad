{-# LANGUAGE OverloadedStrings #-}

module Pulp.Desk.System.Hardware.BatteryStatus (
  BatteryStatus (..),
  BatteryStat (..),
  batteryStat,
) where

import Control.Applicative
import Data.List
import Pulp.Desk.Utils.ParseHor qualified as Parse
import System.Directory
import System.FilePath

-- | Battery status. Note that @NotCharging@ means "Not charging".
data BatteryStatus = Charging | Discharging | NotCharging | Full | Unknown
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
data BatteryStat = MkBatteryStat
  { status :: !BatteryStatus
  , capacity :: !Int
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

-- | Gets Battery statistics.
-- Pulls from </sys/class/power_supply/BAT?/uevent>.
batteryStat :: IO BatteryStat
batteryStat = do
  let path = "/" </> "sys" </> "class" </> "power_supply"
  batName : _ <- filter ("BAT" `isPrefixOf`) <$> listDirectory path
  Parse.parseFile battery (path </> batName </> "uevent")
  where
    battery = Parse.fields (Parse.symbolH "=" *> decOrStr) >>= Parse.exQueryMap query
    decOrStr = Parse.labelH "data" $ Right <$> Parse.decimalH <|> Left <$> Parse.remainH

    query = do
      let asStr = either Just (const Nothing)
          asInt = either (const Nothing) Just
      status <- Parse.queryFieldAs "POWER_SUPPLY_STATUS" (fmap statusEnum . asStr)
      capacity <- Parse.queryFieldAs "POWER_SUPPLY_CAPACITY" asInt
      energyFull <- Parse.queryOptAs "POWER_SUPPLY_ENERGY_FULL" asInt
      chargeFull <- Parse.queryOptAs "POWER_SUPPLY_CHARGE_FULL" asInt
      energyFullDesign <- Parse.queryOptAs "POWER_SUPPLY_ENERGY_FULL_DESIGN" asInt
      chargeFullDesign <- Parse.queryOptAs "POWER_SUPPLY_CHARGE_FULL_DESIGN" asInt
      energyNow <- Parse.queryOptAs "POWER_SUPPLY_ENERGY_NOW" asInt
      chargeNow <- Parse.queryOptAs "POWER_SUPPLY_CHARGE_NOW" asInt
      voltageNow <- Parse.queryOptAs "POWER_SUPPLY_VOLTAGE_NOW" asInt
      voltageMinDesign <- Parse.queryOptAs "POWER_SUPPLY_VOLTAGE_MIN_DESIGN" asInt
      currentNow <- Parse.queryOptAs "POWER_SUPPLY_CURRENT_NOW" asInt
      powerNow <- Parse.queryOptAs "POWER_SUPPLY_POWER_NOW" asInt
      pure MkBatteryStat{..}

    statusEnum = \case
      "Charging" -> Charging
      "Discharging" -> Discharging
      "Not charging" -> NotCharging
      "Full" -> Full
      _ -> Unknown
