{-# LANGUAGE QuantifiedConstraints #-}

module Control.ReactiveSpec (reactiveSpec) where

import Control.Event.Entry
import Control.Event.State
import Data.Foldable
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Proxy
import Data.Set qualified as S
import Data.Vector qualified as V
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

networkPollingDiscrete :: [a] -> Event b -> MomentIO (Event a)
networkPollingDiscrete vals eInp = do
  ref <- liftIO $ newIORef vals
  (eRes, _) <- pollingDiscrete (atomicModifyIORef' ref $ \(x : xs) -> (xs, x)) eInp
  pure eRes

-- TODO Pure non-reactive logic should not be here
instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary :: Arbitrary a => Gen (V.Vector a)
  arbitrary = V.fromList <$> arbitrary

zipToRightSpec ::
  forall t.
  (ZipToRight t, forall a. Eq a => Eq (t a), forall a. Show a => Show (t a), forall a. Arbitrary a => Arbitrary (t a)) =>
  Proxy t ->
  Spec
zipToRightSpec _ = do
  prop "is idempotent" $ \(x :: t Integer) -> zipToRight (,) x x == ((\t -> (Just t, t)) <$> x)
  prop "is aligned" $ \(x :: t Integer) (y :: t Integer) -> toList y == toList (zipToRight (const id) x y)

patchSpec :: forall a g. (Patch g a, Eq a, Show a, Show g, Arbitrary a) => Proxy a -> Spec
patchSpec _ = do
  prop "has transitive action" $ \(x :: a) (y :: a) ->
    let patch = y <-- x
     in counterexample ("with patch " <> show patch) $ (y <-- x) <: x == y

actMorphSpec :: (Act g a, Act h b, Eq b, Show a, Show g, Arbitrary g, Arbitrary a) => (a -> b) -> (g -> h) -> Spec
actMorphSpec fn homo = do
  prop "is an action morphism" $ \g x -> fn (g <: x) == homo g <: fn x

instance Arbitrary op => Arbitrary (PatchOf op) where
  arbitrary :: Arbitrary op => Gen (PatchOf op)
  arbitrary = MkPatchOf <$> arbitrary

-- Since this one should be uniform across calls.
instance Arbitrary (CacheStack Int) where
  arbitrary :: Gen (CacheStack Int)
  arbitrary = AsCacheStack . V.imap (\i () -> i) <$> arbitrary

instance Arbitrary a => Arbitrary (SetOp a) where
  arbitrary :: Arbitrary a => Gen (SetOp a)
  arbitrary = oneof [SetInsert <$> arbitrary, SetDelete <$> arbitrary]

instance (Ord k, Arbitrary k) => Arbitrary (CacheMap k k) where
  arbitrary :: (Ord k, Arbitrary k) => Gen (CacheMap k k)
  arbitrary = AsCacheMap . M.fromSet id <$> arbitrary

instance Arbitrary k => Arbitrary (CachePair k k) where
  arbitrary :: Arbitrary k => Gen (CachePair k k)
  arbitrary = cachePair <$> arbitrary
    where
      cachePair k = MkCachePair k k

reactiveSpec :: Spec
reactiveSpec = do
  describe "Event.Entry" $ do
    describe "diffEvent" $ do
      -- Ignores delays
      prop "reports all changes correctly" $ \x mayXs -> monadicIO $ do
        out <- run $ newIORef []
        run $ interpretFrameworks (networkDiffEvent @Integer out x) mayXs
        actual <- run $ readIORef out
        let xs = catMaybes mayXs
            expected = zip (x : xs) xs
        expectVsActual expected actual

    describe "syncBehavior" $ do
      prop "syncs to all steps correctly" $ \x mayXs -> monadicIO $ do
        out <- run $ newIORef []
        run $ interpretFrameworks (networkSyncBehavior @Integer out x) mayXs
        actual <- run $ readIORef out
        let expected = x : catMaybes mayXs
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

  describe "Entry.State" $ do
    describe "ZipToRight" $ do
      describe "Vector" $ zipToRightSpec (Proxy @V.Vector)
      describe "Map.Strict" $ zipToRightSpec (Proxy @(M.Map Integer))

    describe "Patch" $ do
      describe "CacheStack" $ patchSpec (Proxy @(CacheStack Int))
      describe "Set" $ patchSpec (Proxy @(S.Set Integer))
      describe "CacheMap" $ patchSpec (Proxy @(CacheMap Integer Integer))

    describe "Act Morphism" $ do
      describe "CacheMap->Set" $ actMorphSpec (mapGetCached @Integer @Integer) (mapCachePatch @Integer @Integer)
