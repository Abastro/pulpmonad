{-# LANGUAGE TypeFamilies #-}

module Control.Event.State (
  Discrete,
  exeMapAccum,
  exeAccumD,
  ZipToRight (..),
  zipToRightM,
  Diffs (..),
  SetLike (..),
  withOnRemove,
  computeDiffs,
) where

import Control.Event.Entry
import Data.Foldable
import Data.Map.Merge.Strict qualified as M
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Vector qualified as V

-- | Aligns and zips to fit the right collection, padding left ones with Nothing. (c.f. SemiAlign acts like union)
--
-- [Idempotency] @zipToRight (,) x x = (\t -> (Just t, t)) <$> x@
-- [Alignedness] @toList y = toList (zipToRight (const id) x y)@
-- [Functor Composition] @zipToRight (,) (f <$> x) (g <$> y) = bimap (fmap f) g <$> zipToRight (,) x y@
--
-- Parametricity will enforce the following property.
--
-- [With] @zipToRight f x y = uncurry f <$> zipToRight (,) x y@
class (Functor t, Foldable t) => ZipToRight t where
  zipToRight :: (Maybe a -> b -> c) -> t a -> t b -> t c

instance ZipToRight V.Vector where
  zipToRight :: (Maybe a -> b -> c) -> V.Vector a -> V.Vector b -> V.Vector c
  zipToRight fn left = V.imap $ \idx -> fn (left V.!? idx)

instance Ord k => ZipToRight (M.Map k) where
  zipToRight :: Ord k => (Maybe a -> b -> c) -> M.Map k a -> M.Map k b -> M.Map k c
  zipToRight fn = M.merge M.dropMissing onlyRight both
    where
      onlyRight = M.mapMissing $ \_key -> fn Nothing
      both = M.zipWithMatched $ \_key l -> fn (Just l)

-- | Zip to right with an applicative.
zipToRightM :: (Applicative f, Traversable t, ZipToRight t) => (Maybe a -> b -> f c) -> t a -> t b -> f (t c)
zipToRightM fn l r = sequenceA (zipToRight fn l r)

-- | Difference of a container, represented by pair of removed and added stuffs.
data Diffs a = Diffs {removed :: a, added :: a}
  deriving (Show, Functor)

-- | Patching action on the target type.
--
-- Inspired by torsor, generalized to apply when the acting type is not closed on the group operator.
--
-- The acting type should have a closure with a proper group structure,
-- which acts transitively onto the target type.
--
-- Uniqueness of patching action is not needed, you simply need to pick any.
--
-- [Transitivity] @(y <-- x) <: x = y@
class Patch g a where
  -- Performs action.
  (<:) :: g -> a -> a

  -- Patching action transforming specific values.
  (<--) :: a -> a -> g

  infixr 5 <:
  infix 7 <--

-- | Cache vector, where each element only depends on position.
newtype CacheVec a = MkCacheVec (V.Vector a)

-- | Positional action on cache vector.
data CacheVecPatch a = Appends !(V.Vector a) | Deducts !(V.Vector a)
  deriving (Show)

instance Eq a => Eq (CacheVecPatch a) where
  (==) :: Eq a => CacheVecPatch a -> CacheVecPatch a -> Bool
  Appends v == Appends w = v == w
  -- Special treatment needed for empty append/deduct
  Appends v == Deducts w = null v && null w
  Deducts v == Appends w = null v && null w
  Deducts v == Deducts w = v == w

instance Patch (CacheVecPatch a) (V.Vector a) where
  (<:) :: CacheVecPatch a -> V.Vector a -> V.Vector a
  Appends v <: old = old <> v
  Deducts v <: old = V.take (V.length old - V.length v) old

  (<--) :: V.Vector a -> V.Vector a -> CacheVecPatch a
  new <-- old = case V.length old `compare` V.length new of
    LT -> Appends (V.drop (V.length old) new)
    EQ -> Appends V.empty
    GT -> Deducts (V.drop (V.length new) old)

-- | Collection with union(\/), intersection(/\) and difference(\\).
--
-- This does not imply that there should be unique value involved.
--
-- Operation also need not be commutative.
--
-- [Associativity] @x '\/' (y '\/' z) = (x '\/' y) '\/' z@, @x '/\' (y '/\' z) = (x '/\' y) '/\' z@
-- [Partition] @(x '/\' y) '\/' (x '\\' y) = x@
-- [Double negation] @x '\\' (x '\\' y) = x '/\' y@
class SetLike m where
  (\/) :: m -> m -> m
  (/\) :: m -> m -> m
  (\\) :: m -> m -> m

  infixl 5 \/
  infixl 6 /\
  infixl 9 \\

-- MAYBE there are more laws

instance SetLike (V.Vector a) where
  (\/) :: V.Vector a -> V.Vector a -> V.Vector a
  x \/ y = x <> y

  (/\) :: V.Vector a -> V.Vector a -> V.Vector a
  x /\ y = V.take (V.length y) x

  -- FIXME Double negation falsified, alternative approach needed to generalize.
  -- Namely, '\\' violates the positional contracts.
  (\\) :: V.Vector a -> V.Vector a -> V.Vector a
  x \\ y = V.drop (V.length y) x

instance Ord k => SetLike (S.Set k) where
  (\/) :: Ord k => S.Set k -> S.Set k -> S.Set k
  (\/) = S.union

  (/\) :: Ord k => S.Set k -> S.Set k -> S.Set k
  (/\) = S.intersection

  (\\) :: Ord k => S.Set k -> S.Set k -> S.Set k
  (\\) = S.difference

instance Ord k => SetLike (M.Map k a) where
  (\/) :: Ord k => M.Map k a -> M.Map k a -> M.Map k a
  (\/) = M.union

  (/\) :: Ord k => M.Map k a -> M.Map k a -> M.Map k a
  (/\) = M.intersection

  (\\) :: Ord k => M.Map k a -> M.Map k a -> M.Map k a
  (\\) = M.difference

-- | Attach removal action to update function.
withOnRemove :: (Monad m, Foldable t, SetLike (t a)) => (a -> m ()) -> (t a -> m (t a)) -> (t a -> m (t a))
withOnRemove delete update old = do
  new <- update old
  new <$ traverse_ delete (old \\ new)

-- | Compute differences.
-- When parameters are @x@ and @y@, this requires @x /\ y = y /\ x@ for recovery using this diffs.
computeDiffs :: SetLike m => m -> m -> Diffs m
computeDiffs old new = Diffs{removed = old \\ new, added = new \\ old}
