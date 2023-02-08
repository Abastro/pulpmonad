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

stateSpec :: Spec
stateSpec = do
  describe "Entry.State" $ do
    describe "ZipToRight" $ do
      describe "Vector" $ zipToRightSpec (Proxy @V.Vector)
      describe "Map.Strict" $ zipToRightSpec (Proxy @(M.Map Integer))

    describe "Patch" $ do
      describe "CacheStack" $ patchSpec (Proxy @(CacheStack Int))
      describe "Maybe" $ patchSpec (Proxy @(Maybe Integer))
      describe "Set" $ patchSpec (Proxy @(S.Set Integer))
      describe "CacheMap" $ patchSpec (Proxy @(CacheMap Integer Integer))

    describe "Act Morphism" $ do
      describe "CacheMap->Set" $ actMorphSpec (mapGetCached @Integer @Integer) (mapCachePatch @Integer @Integer)
