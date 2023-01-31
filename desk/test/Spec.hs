module Main (main) where

import Control.Monad
import Reactive.Banana.Combinators hiding (once)
import Reactive.Banana.Frameworks
import Status.AudioStatus
import Status.HWStatus
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Event.Entry
import Data.Maybe

-- | Make future event into plain - also introduces delay to event
asPlain :: Event (Future a) -> MomentIO (Event a)
asPlain eFuture = do
  (ePlain, handle) <- newEvent
  reactimate' $ fmap handle <$> eFuture
  pure ePlain

networkDiffEvent :: a -> Event a -> MomentIO (Event (a, a))
networkDiffEvent x0 ex = do
  bx  <- stepper x0 ex
  -- No need to check arbitrary function due to parametricity, I believe
  asPlain =<< updateEvent (,) bx bx

main :: IO ()
main = hspec $ do
  describe "Control.Event.Entry" $ do
    describe "diffEvent" $ do
      prop "reports all changes correctly" $ \x mayXs -> monadicIO $ do
        mayYs <- run $ interpretFrameworks (networkDiffEvent @Int x) mayXs
        -- Collapse maybes to ignore delays
        let xs = catMaybes mayXs
            expected = zip (x : xs) xs
            actual = catMaybes mayYs
        assert $ expected == actual

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
