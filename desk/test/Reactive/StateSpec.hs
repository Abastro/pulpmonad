{-# LANGUAGE QuantifiedConstraints #-}

module Reactive.StateSpec (stateSpec) where

import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Proxy
import Data.Set qualified as S
import Data.Vector qualified as V
import Pulp.Desk.Reactive.State
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary :: Arbitrary a => Gen (V.Vector a)
  arbitrary = V.fromList <$> arbitrary

  shrink :: Arbitrary a => V.Vector a -> [V.Vector a]
  shrink = shrinkMap V.fromList V.toList

zipToRightSpec ::
  forall t.
  (ZipToRight t, forall a. Eq a => Eq (t a), forall a. Show a => Show (t a), forall a. Arbitrary a => Arbitrary (t a)) =>
  Proxy t ->
  Spec
zipToRightSpec _ = do
  prop "is idempotent" $ withMaxSuccess 500 $ \(x :: t Integer) -> zipToRight (,) x x == ((\t -> (Just t, t)) <$> x)
  prop "is aligned" $ withMaxSuccess 500 $ \(x :: t Integer) (y :: t Integer) -> toList y == toList (zipToRight (const id) x y)

patchSpec :: forall a g. (Patch g a, Eq a, Show a, Show g, Arbitrary a) => Proxy a -> Spec
patchSpec _ = do
  prop "has transitive action" $ withMaxSuccess 500 $ \(x :: a) (y :: a) ->
    let patch = y <-- x
     in counterexample ("with patch " <> show patch <> " which gives " <> show (patch <: x)) $ patch <: x == y

diffPatchingSpec :: forall g a b. (DiffPatching g a b, Eq b, Show a, Arbitrary a) => Proxy a -> Proxy b -> Spec
diffPatchingSpec _ _ = do
  prop "can recover patch from difference" $ withMaxSuccess 500 $ \(x :: a) (y :: a) ->
    (directMap y :: b) == (y <-- x) <: directMap x

instance Arbitrary op => Arbitrary (PatchOf op) where
  arbitrary :: Arbitrary op => Gen (PatchOf op)
  arbitrary = MkPatchOf <$> arbitrary

  shrink :: Arbitrary op => PatchOf op -> [PatchOf op]
  shrink = shrinkMap MkPatchOf (\(MkPatchOf v) -> v)

instance Arbitrary a => Arbitrary (ColOp a) where
  arbitrary :: Arbitrary a => Gen (ColOp a)
  arbitrary = oneof [Insert <$> arbitrary, Delete <$> arbitrary]

  shrink :: Arbitrary a => ColOp a -> [ColOp a]
  shrink = \case
    Insert x -> [Insert y | y <- shrink x]
    Delete x -> [Delete y | y <- shrink x]

-- Since this one should be uniform across calls.
-- TODO shrink?
instance Arbitrary (CacheStack Int) where
  arbitrary :: Gen (CacheStack Int)
  arbitrary = AsCacheStack . V.imap (\i () -> i) <$> arbitrary

instance (Ord k, Arbitrary k) => Arbitrary (CacheMap k k) where
  arbitrary :: (Ord k, Arbitrary k) => Gen (CacheMap k k)
  arbitrary = AsCacheMap . M.fromSet id <$> arbitrary

stateSpec :: Spec
stateSpec = do
  describe "ZipToRight" $ do
    describe "Vector" $ zipToRightSpec (Proxy @V.Vector)
    describe "Map.Strict" $ zipToRightSpec (Proxy @(M.Map Integer))

  -- Uses Int: Seems better at testing duplicate values
  describe "Patch" $ do
    describe "Set" $ patchSpec (Proxy @(S.Set Int))
    describe "Vector" $ patchSpec (Proxy @(V.Vector Int))

  describe "DiffPatching" $ do
    describe "Maybe->Set" $ diffPatchingSpec (Proxy @(Maybe Int)) (Proxy @(S.Set Int))
    describe "Vector->Set" $ diffPatchingSpec (Proxy @(CacheStack Int)) (Proxy @(V.Vector Int))
    describe "CacheMap->Set" $ diffPatchingSpec (Proxy @(CacheMap Int Int)) (Proxy @(S.Set Int))
