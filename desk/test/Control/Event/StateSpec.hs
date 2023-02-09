{-# LANGUAGE QuantifiedConstraints #-}

module Control.Event.StateSpec (stateSpec) where

import Control.Event.State
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Proxy
import Data.Set qualified as S
import Data.Vector qualified as V
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

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
     in counterexample ("with patch " <> show patch <> " which gives " <> show (patch <: x)) $ patch <: x == y

diffPatchingSpec :: forall g a b. (DiffPatching g a b, Eq b, Show a, Arbitrary a) => Proxy a -> Proxy b -> Spec
diffPatchingSpec _ _ = do
  prop "can recover patch from difference" $ \(x :: a) (y :: a) ->
    (directMap y :: b) == (y <-- x) <: directMap x

instance Arbitrary op => Arbitrary (PatchOf op) where
  arbitrary :: Arbitrary op => Gen (PatchOf op)
  arbitrary = MkPatchOf <$> arbitrary

instance Arbitrary a => Arbitrary (ColOp a) where
  arbitrary :: Arbitrary a => Gen (ColOp a)
  arbitrary = oneof [Insert <$> arbitrary, Delete <$> arbitrary]

-- Since this one should be uniform across calls.
instance Arbitrary (CacheStack Int) where
  arbitrary :: Gen (CacheStack Int)
  arbitrary = AsCacheStack . V.imap (\i () -> i) <$> arbitrary

instance (Ord k, Arbitrary k) => Arbitrary (CacheMap k k) where
  arbitrary :: (Ord k, Arbitrary k) => Gen (CacheMap k k)
  arbitrary = AsCacheMap . M.fromSet id <$> arbitrary

stateSpec :: Spec
stateSpec = do
  describe "Entry.State" $ do
    describe "ZipToRight" $ do
      describe "Vector" $ zipToRightSpec (Proxy @V.Vector)
      describe "Map.Strict" $ zipToRightSpec (Proxy @(M.Map Integer))

    describe "Patch" $ do
      describe "Set" $ patchSpec (Proxy @(S.Set Integer))
      describe "Vector" $ patchSpec (Proxy @(V.Vector Integer))

    describe "DiffPatching" $ do
      describe "Maybe->Set" $ diffPatchingSpec (Proxy @(Maybe Integer)) (Proxy @(S.Set Integer))
      describe "Vector->Set" $ diffPatchingSpec (Proxy @(CacheStack Int)) (Proxy @(V.Vector Int))
      describe "CacheMap->Set" $ diffPatchingSpec (Proxy @(CacheMap Integer Integer)) (Proxy @(S.Set Integer))
