{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Event.State (
  Discrete,
  exeMapAccum,
  exeAccumD,
  ZipToRight (..),
  zipToRightM,
  remainderRight,
  onRemainderRight,
  Act (..),
  Diff (..),
  (-->),
  Patch,
  DiffPatching (..),
  PatchOf (..),
  applyPatches,
  applyImpure,
  ColOp (..),
  PatchCol,
  CacheStack (..),
  CacheMap (..),
) where

import Control.Event.Entry
import Data.Coerce
import Data.Foldable
import Data.Map.Merge.Strict qualified as M
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Vector qualified as V
import qualified Data.Vector.Generic as VG

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

-- | Remainder of right ones which does not match with left.
remainderRight :: ZipToRight t => t a -> t b -> t (Maybe b)
remainderRight = zipToRight $ \case
  Just _ -> const Nothing
  Nothing -> Just

-- TODO onRemainder could be confusing on parameter order.

-- | Act on the remainder of right ones not matching with left.
onRemainderRight :: (Applicative f, ZipToRight t) => (b -> f ()) -> t a -> t b -> f ()
onRemainderRight act = fmap (traverse_ fn) . remainderRight
  where
    fn = maybe (pure ()) act

-- TODO "Act" better be Monoid action.

-- | Action on the target type.
--
-- It should be a monoid action, but the acting type might not be closed on the monoid operator.
--
-- When acting type is a semigroup, it should satisfy
--
-- [Composition] @g <: (h <: x) = (g <> h) <: x@
--
-- When acting type is a monoid, it should satisfy
--
-- [Identity] @mempty <: x = x@
class Act g a where
  -- | Performs action.
  (<:) :: g -> a -> a

  infixr 5 <:

-- MAYBE Diff class is a horrible idea.

-- | A difference type of the target type.
--
-- This typeclass exists for overloading, so it has no laws.
class Diff g a | a -> g where
  -- | Computes difference.
  (<--) :: a -> a -> g

  infix 7 <--

(-->) :: Diff g a => a -> a -> g
(-->) = flip (<--)
infix 7 -->

-- | Patching action on the target type, where difference gives the patching action.
--
-- Inspired by torsor, generalized to apply when the acting type is not closed on the group operator.
--
-- The acting type should have a closure with a proper group structure,
-- which acts transitively onto the target type.
--
-- Uniqueness of patching action is not needed, you simply need to pick any.
--
-- [Transitivity] @(y <-- x) <: x = y@
class (Act g a, Diff g a) => Patch g a | a -> g

-- | Patching where source type difference translates to target type patching action.
--
-- Useful for updating a large data structure by parts to match other large data.
--
-- The patching action of target type is not guaranteed to be the same as the difference of source type,
--
-- i.e. given @x@ and @y@, @directMap y <-- directMap x@ may not be @y <-- x@.
--
-- [Recovery] @directMap y = (y <-- x) <: directMap x@
class (Diff g a, Patch g b) => DiffPatching g a b | a -> g, b -> g where
  directMap :: a -> b

-- | Patch by list of operations, applied from front to back (left to right).
newtype PatchOf op = MkPatchOf (V.Vector op)
  deriving (Eq, Show, Functor)

applyPatches :: (op -> m -> m) -> (PatchOf op -> m -> m)
applyPatches apply (MkPatchOf ops) x = foldl' (flip apply) x ops

applyImpure :: Applicative m => (op -> m ()) -> (PatchOf op -> m ())
applyImpure asAct (MkPatchOf ops) = traverse_ asAct ops

-- | Basic collection operations.
data ColOp a = Insert !a | Delete !a
  deriving (Eq, Show, Functor)

type PatchCol a = PatchOf (ColOp a)

instance Ord a => Act (PatchCol a) (S.Set a) where
  (<:) :: Ord a => PatchCol a -> S.Set a -> S.Set a
  (<:) = applyPatches $ \case
    Insert x -> S.insert x
    Delete x -> S.delete x

instance Ord a => Diff (PatchCol a) (S.Set a) where
  (<--) :: Ord a => S.Set a -> S.Set a -> PatchCol a
  new <-- old = MkPatchOf . V.fromList $ (Delete <$> deleted) <> (Insert <$> inserted)
    where
      inserted = S.toList $ new S.\\ old
      deleted = S.toList $ old S.\\ new

instance Ord a => Patch (PatchCol a) (S.Set a)

-- Inserts at last, Deletes from anywhere.
instance Eq a => Act (PatchCol a) (V.Vector a) where
  (<:) :: Eq a => PatchCol a -> V.Vector a -> V.Vector a
  (<:) = applyPatches $ \case
    Insert x -> (`V.snoc` x)
    Delete x -> deleteOneFromRight x
    where
      deleteOneFromRight x vec = case VG.findIndexR (== x) vec of
        Nothing -> vec
        Just idx -> V.take idx vec <> V.drop (succ idx) vec

-- Rudimentary difference evaluation.
instance Eq a => Diff (PatchCol a) (V.Vector a) where
  (<--) :: Eq a => V.Vector a -> V.Vector a -> PatchCol a
  new <-- old = MkPatchOf $ (Delete <$> deleted) <> (Insert <$> inserted)
    where
      -- Deleting order does not matter
      deleted = V.drop sameLen old
      inserted = V.drop sameLen new
      sameLen = length . takeWhile id $ zipWith (==) (V.toList old) (V.toList new)

instance Eq a => Patch (PatchCol a) (V.Vector a)

-- Maybe can be mapped to Set.
instance Diff (PatchCol a) (Maybe a) where
  (<--) :: Maybe a -> Maybe a -> PatchCol a
  new <-- old = MkPatchOf . V.fromList $ (Delete <$> toList old) <> (Insert <$> toList new)

instance Ord a => DiffPatching (PatchCol a) (Maybe a) (S.Set a) where
  directMap :: Maybe a -> S.Set a
  directMap = S.fromAscList . toList

-- | Cache stack, where each element depends on its position.
-- Right side is the top.
newtype CacheStack a = AsCacheStack (V.Vector a)
  deriving (Eq, Show, Semigroup, Monoid)

instance Diff (PatchCol a) (CacheStack a) where
  (<--) :: CacheStack a -> CacheStack a -> PatchCol a
  AsCacheStack new <-- AsCacheStack old = MkPatchOf $
    case V.length old `compare` V.length new of
      LT -> Insert <$> V.drop (V.length old) new
      EQ -> V.empty
      GT -> Delete <$> V.drop (V.length new) old

instance Eq a => DiffPatching (PatchCol a) (CacheStack a) (V.Vector a) where
  directMap :: CacheStack a -> V.Vector a
  directMap = coerce

-- | Cache map, where each element is uniquely determined by the key.
newtype CacheMap k v = AsCacheMap (M.Map k v)
  deriving (Eq, Show, Semigroup, Monoid)

-- CacheMap is mapped into the value set. It is what I use.
instance Ord k => Diff (PatchCol v) (CacheMap k v) where
  (<--) :: Ord k => CacheMap k v -> CacheMap k v -> PatchCol v
  AsCacheMap new <-- AsCacheMap old = MkPatchOf . V.fromList $ (Delete <$> deleted) <> (Insert <$> inserted)
    where
      inserted = M.elems $ new M.\\ old
      deleted = M.elems $ old M.\\ new

instance (Ord k, Ord v) => DiffPatching (PatchCol v) (CacheMap k v) (S.Set v) where
  directMap :: Ord k => CacheMap k v -> S.Set v
  directMap = coerce (S.fromList . M.elems)
