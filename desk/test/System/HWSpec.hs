module System.HWSpec (hwSpec) where

import Control.Monad
import Pulp.Desk.System.AudioStatus
import Pulp.Desk.System.Hardware.CPUStatus
import Pulp.Desk.System.Hardware.DiskStatus
import Pulp.Desk.System.Hardware.MemoryStatus
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Pulp.Desk.System.Hardware.BatteryStatus

-- FIXME This requires the desktop env, better spec needed
hwSpec :: Spec
hwSpec = do
  describe "Hardware" $ do
    describe "CPUStatus" $ do
      describe "cpuStat" $ do
        it "does not throw" $ void cpuStat
      describe "cpuTemperature" $ do
        it "does not throw" $ void cpuTemperature

    describe "MemoryStatus" $ do
      describe "memoryStat" $ do
        it "does not throw" $ void memoryStat
        prop "has used ratio in range [0, 1]" . once $
          monadicIO $ do
            used <- run (memoryUsed . memoryRatios <$> memoryStat)
            assert (used >= 0 && used <= 1)

    describe "BatteryStatus" $ do
      describe "batteryStat" $ do
        it "does not throw" $ void batteryStat

    describe "DiskStatus" $ do
      describe "diskStat" $ do
        it "does not throw" $ void diskStat

  describe "AudioStatus" $ do
    describe "curVolStat" $ do
      prop "should return valid values for defaults" . once $ monadicIO $ do
        Just VolStat{..} <- run (curVolStat "default" "Master")
        assert (curVolume >= 0 && curVolume <= 1)