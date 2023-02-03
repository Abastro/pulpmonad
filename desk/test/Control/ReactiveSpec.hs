module Control.ReactiveSpec (reactiveSpec) where

import Control.Event.Entry
import Data.IORef
import Data.Maybe
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

appendRef :: IORef [a] -> Sink a
appendRef ref x = modifyIORef' ref (<> [x])

expectVsActual :: (Eq a, Show a) => a -> a -> PropertyM IO ()
expectVsActual expected actual = do
  monitor (counterexample $ "Expected: " <> show expected <> ", Actual: " <> show actual)
  assert $ expected == actual

networkDiffEvent :: IORef [(a, a)] -> a -> Event a -> MomentIO (Event ())
networkDiffEvent out x0 ex = do
  bx <- stepper x0 ex
  -- No need to check arbitrary function due to parametricity, I believe
  eDiff <- diffEvent (,) bx
  reactimate' $ fmap (appendRef out) <$> eDiff
  pure never

networkSyncBehavior :: IORef [a] -> a -> Event a -> MomentIO (Event ())
networkSyncBehavior out x0 ex = do
  bx <- stepper x0 ex
  syncBehavior bx $ appendRef out
  pure never

reactiveSpec :: Spec
reactiveSpec = do
  describe "Event.Entry" $ do
    describe "diffEvent" $ do
      -- Ignores delays
      prop "reports all changes correctly" $ \x mayXs -> monadicIO $ do
        out <- run $ newIORef []
        run $ interpretFrameworks (networkDiffEvent @Int out x) mayXs
        actual <- run $ readIORef out
        let xs = catMaybes mayXs
            expected = zip (x : xs) xs
        expectVsActual expected actual

    describe "syncBehavior" $ do
      prop "syncs to all steps correctly" $ \x mayXs -> monadicIO $ do
        out <- run $ newIORef []
        run $ interpretFrameworks (networkSyncBehavior @Int out x) mayXs
        actual <- run $ readIORef out
        let expected = x : catMaybes mayXs
        expectVsActual expected actual
