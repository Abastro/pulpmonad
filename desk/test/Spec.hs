module Main (main) where

import HWStatus
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = hspec $ do
  describe "HWStatus" $ do
    describe "cpuStat" $ do
      it "does not throw" $ () <$ cpuStat
    describe "cpuDiff" $ do
      prop "does not throw for positive delays" . withMaxSuccess 10 $
        \(Positive t) -> () <$ cpuDiff t
      prop "has used ratio in range [0, 1]" . withMaxSuccess 10 $
        \(Positive t) -> monadicIO $ do
          used <- run (cpuUsedRatio <$> cpuDiff t)
          assert (used >= 0 && used <= 1)
    describe "cpuTemp" $ do
      it "does not throw" $ () <$ cpuTemp
    describe "memStat" $ do
      it "does not throw" $ () <$ memStat
      prop "has used ratio in range [0, 1]" . once $
        monadicIO $ do
          used <- run (memUsedRatio <$> memStat)
          assert (used >= 0 && used <= 1)
    describe "batStat" $ do
      it "does not throw" $ () <$ batStat
