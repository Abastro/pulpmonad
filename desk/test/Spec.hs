module Main (main) where

import Control.Monad
import Status.HWStatus
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Status.AudioStatus

main :: IO ()
main = hspec $ do
  describe "HWStatus" $ do
    describe "cpuStat" $ do
      it "does not throw" $ void cpuStat
    -- TODO Count number of CPUs
    describe "cpuDelta" $ do
      prop "does not throw for positive delays" . withMaxSuccess 10 $
        \(Positive t) -> void $ cpuDelta t
      prop "has used ratio in range [0, 1]" . withMaxSuccess 10 $
        \(Positive t) -> monadicIO $ do
          used <- run (cpuUsed . cpuRatios <$> cpuDelta t)
          assert (used >= 0 && used <= 1)
    describe "cpuTemp" $ do
      it "does not throw" $ void cpuTemp
    describe "memStat" $ do
      it "does not throw" $ void memStat
      prop "has used ratio in range [0, 1]" . once $
        monadicIO $ do
          used <- run (memUsed . memRatios <$> memStat)
          assert (used >= 0 && used <= 1)
    describe "batStat" $ do
      it "does not throw" $ void batStat
    describe "diskStat" $ do
      it "does not throw" $ void diskStat
    describe "diskDelta" $ do
      prop "does not throw for positive delays" . withMaxSuccess 10 $
        \(Positive t) -> void $ diskDelta t

  describe "AudioStatus" $ do
    describe "curVolStat" $ do
      prop "should return valid values for defaults" . once $ monadicIO $ do
        Just VolStat{..} <- run (curVolStat "default" "Master")
        assert (curVolume >= 0 && curVolume <= 1)
