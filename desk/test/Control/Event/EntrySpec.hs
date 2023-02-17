module Control.Event.EntrySpec (entrySpec) where

import Control.Concurrent
import Control.Event.Entry
import Control.Exception (evaluate)
import Control.Monad (void)
import Data.IORef
import Data.Maybe
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import System.Mem
import System.Mem.Weak
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (once)
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

networkExeMapAccum :: IORef [acc] -> acc -> Event (acc -> MomentIO (sig, acc)) -> MomentIO (Event sig)
networkExeMapAccum out acc eUpdate = do
  (eSig, bAcc) <- exeMapAccum acc eUpdate
  -- TODO Better tracking of behavior
  syncBehavior bAcc $ appendRef out
  pure eSig

networkMapAccum :: IORef [acc] -> acc -> Event (acc -> (sig, acc)) -> MomentIO (Event sig)
networkMapAccum out acc eUpdate = do
  (eSig, bAcc) <- mapAccum acc eUpdate
  syncBehavior bAcc $ appendRef out
  pure eSig

networkPollingDiscrete :: [a] -> Event b -> MomentIO (Event a)
networkPollingDiscrete vals eInp = do
  ref <- liftIO $ newIORef vals
  (eRes, _) <- pollingDiscrete (atomicModifyIORef' ref $ \(x : xs) -> (xs, x)) eInp
  pure eRes

data ReactGC = Signal | Finish | Detect deriving (Eq, Show)

networkReactEvent :: Event ReactGC -> MomentIO (Event Bool)
networkReactEvent event = do
  let eSignal = filterE (== Signal) event
      eFinish = filterE (== Finish) event
      eDetect = filterE (== Detect) event
  -- Enclose key inside.
  (det, finish) <-
    liftIO (show <$> getNumCapabilities) >>= \key -> do
      let eAct = void (evaluate key) <$ eSignal
      finish <- reactEvent eAct
      det <- liftIO $ mkWeakPtr key Nothing -- Make weak refs to detect precense.
      pure (det, finish)
  reactimate $ finish <$ eFinish -- React to finish action outside.
  mapEventIO (const $ detectUnreachable det) eDetect -- Detect if it is freed.
  where
    detectUnreachable det = do
      -- reactEvent calls forkIO to call network and decouple event from reactimate.
      -- It is fine if it was delayed, but to test it we need to wait for the other thread to finish.
      -- This is achieved by calling 'yield'.
      yield
      -- Sadly, detecting if a reference can be freed requires major GC.
      performMajorGC
      isNothing <$> deRefWeak det

entrySpec :: Spec
entrySpec = do
  describe "diffEvent" $ do
    -- Ignores delays
    prop "reports all changes correctly" $ \x mayXs -> monadicIO $ do
      out <- run $ newIORef []
      run $ interpretFrameworks (networkDiffEvent @Integer out x) mayXs
      actual <- run $ readIORef out
      let xs = catMaybes mayXs
          expected = zip (x : xs) xs
      expectVsActual expected actual

  -- MAYBE Test loopSource?

  describe "syncBehavior" $ do
    prop "syncs to all steps correctly" $ \x mayXs -> monadicIO $ do
      out <- run $ newIORef []
      run $ interpretFrameworks (networkSyncBehavior @Integer out x) mayXs
      actual <- run $ readIORef out
      let expected = x : catMaybes mayXs
      expectVsActual expected actual

  describe "exeMapAccum" $ do
    -- We generate update function once, which maps event stream from list.
    prop "matches mapAccum for pure updates" $ \(Fn2 update) x mayYs -> monadicIO $ do
      let inE = map (fmap @Maybe @Integer update) mayYs
      outB1 <- run $ newIORef []
      outE1 <- run $ interpretFrameworks (networkMapAccum @Integer @Integer outB1 x) inE
      expected <- run $ (outE1,) <$> readIORef outB1

      -- Pure updates.
      outB2 <- run $ newIORef []
      outE2 <- run $ interpretFrameworks (networkExeMapAccum outB2 x . fmap (pure .)) inE
      actual <- run $ (outE2,) <$> readIORef outB2

      expectVsActual expected actual

    -- FIXME This test does not check concurrent behaviors
    prop "preserves behavior on delay" . withMaxSuccess 20 $ \(Fn2 update) x mayYs -> monadicIO $ do
      let asPure (out, _delay) = pure out
          asDelay (out, Small delay) = out <$ liftIO (threadDelay delay)

      let inE = map (fmap @Maybe @Integer update) mayYs
      outB1 <- run $ newIORef []
      outE1 <- run $ interpretFrameworks (networkExeMapAccum @Integer @Integer outB1 x . fmap (asPure .)) inE
      expected <- run $ (outE1,) <$> readIORef outB1

      outB2 <- run $ newIORef []
      outE2 <- run $ interpretFrameworks (networkExeMapAccum outB2 x . fmap (asDelay .)) inE
      actual <- run $ (outE2,) <$> readIORef outB2

      expectVsActual expected actual

  describe "pollingDescrete" $ do
    -- TODO Test behavior as well
    -- Also ignore delays
    prop "runs action and emit its result on each input event" $ \(InfiniteList xs _) mayEs -> monadicIO $ do
      mayRs <- run $ interpretFrameworks (networkPollingDiscrete @Integer @Integer xs) mayEs
      let _ : later = xs
          expected = take (length $ catMaybes mayEs) later
          actual = catMaybes mayRs
      expectVsActual expected actual

  -- FIXME This stopped working, inspect 'reactEvent' changes
  -- Test if reactimate is GC-ed on event GC.
  describe "reactEvent" $ do
    prop "correctly frees react content when free is called" . withMaxSuccess 20 $ \fstE sndE ->
      monadicIO $ do
        let withSignal = map . fmap $ \() -> Signal
            inE = withSignal fstE <> [Just Finish] <> withSignal sndE <> [Just Detect]
        monitor (counterexample $ "Input: " <> show inE)
        outE <- run $ interpretFrameworks networkReactEvent inE
        let actual = head $ catMaybes outE
        expectVsActual True actual

-- NOTE: Sadly, simple reactimate is not GC-ed.
