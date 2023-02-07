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
  Patch (..),
  PatchOf (..),
  applyPatches,
  applyImpure,
  CacheStack (..),
  StackOp (..),
  SetOp (..),
  CacheMap (..),
  CachePair (..),
  CacheMapOp,
  mapGetCached,
  mapCachePatch,
) where

import Control.Event.Entry
import Data.Coerce
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
class Act g a => Patch g a | a -> g where
  -- | Patching action transforming specific values.
  (<--) :: a -> a -> g

  infix 7 <--

-- | Patch by list of operations, applied from front to back.
newtype PatchOf op = MkPatchOf (V.Vector op)
  deriving (Eq, Show, Functor)

applyPatches :: (op -> m -> m) -> (PatchOf op -> m -> m)
applyPatches apply (MkPatchOf ops) x = foldl' (flip apply) x ops

applyImpure :: Applicative m => (op -> m ()) -> (PatchOf op -> m ())
applyImpure asAct (MkPatchOf ops) = traverse_ asAct ops

-- | Cache stack, where each element depends on its position.
-- Right side is the top.
newtype CacheStack a = AsCacheStack (V.Vector a)
  deriving (Eq, Show)

-- | Structure operation on a vector from the right(latter) side.
data StackOp a = Push !a | Pop
  deriving (Eq, Show, Functor)

-- Need to consider if stack operation is proper as default action.
instance Act (PatchOf (StackOp a)) (CacheStack a) where
  (<:) :: PatchOf (StackOp a) -> CacheStack a -> CacheStack a
  (<:) = applyPatches . (coerce .) $ \case
    Push x -> (`V.snoc` x)
    Pop -> \v -> V.take (pred $ V.length v) v

instance Patch (PatchOf (StackOp a)) (CacheStack a) where
  (<--) :: CacheStack a -> CacheStack a -> PatchOf (StackOp a)
  AsCacheStack new <-- AsCacheStack old = MkPatchOf $
    case V.length old `compare` V.length new of
      LT -> Push <$> V.drop (V.length old) new
      EQ -> V.empty
      GT -> Pop <$ V.drop (V.length new) old

-- | Structure operation on a set.
data SetOp a = SetInsert !a | SetDelete !a
  deriving (Eq, Show, Functor)

instance Ord a => Act (PatchOf (SetOp a)) (S.Set a) where
  (<:) :: Ord a => PatchOf (SetOp a) -> S.Set a -> S.Set a
  (<:) = applyPatches $ \case
    SetInsert x -> S.insert x
    SetDelete x -> S.delete x

instance Ord a => Patch (PatchOf (SetOp a)) (S.Set a) where
  (<--) :: Ord a => S.Set a -> S.Set a -> PatchOf (SetOp a)
  new <-- old = MkPatchOf . V.fromList $ (SetInsert <$> inserted) <> (SetDelete <$> deleted)
    where
      inserted = S.toList $ new S.\\ old
      deleted = S.toList $ old S.\\ new

-- | Cache map, where each element is uniquely determined by the key.
newtype CacheMap k v = AsCacheMap (M.Map k v)
  deriving (Eq, Show)

-- | Pair of key and corresponding value to cache.
data CachePair k v = MkCachePair !k !v
  deriving (Eq, Show)

-- | Structure operation on cache map is isomorphic to operation on pair set.
type CacheMapOp k v = SetOp (CachePair k v)

-- TODO Multiset-based approach should have been more natural.

instance Ord k => Act (PatchOf (CacheMapOp k v)) (CacheMap k v) where
  (<:) :: Ord k => PatchOf (CacheMapOp k v) -> CacheMap k v -> CacheMap k v
  (<:) = applyPatches $ (coerce .) $ \case
    SetInsert (MkCachePair k v) -> M.insert k v
    SetDelete (MkCachePair k _) -> M.delete k

instance Ord k => Patch (PatchOf (CacheMapOp k v)) (CacheMap k v) where
  (<--) :: Ord k => CacheMap k v -> CacheMap k v -> PatchOf (CacheMapOp k v)
  AsCacheMap new <-- AsCacheMap old = MkPatchOf . V.fromList $ (SetInsert <$> inserted) <> (SetDelete <$> deleted)
    where
      mapToCPair = map (uncurry MkCachePair) . M.toList
      inserted = mapToCPair $ new M.\\ old
      deleted = mapToCPair $ old M.\\ new

-- | All values in the cache map.
mapGetCached :: Ord v => CacheMap k v -> S.Set v
mapGetCached = S.fromList . M.elems . coerce

-- | Maps CacheMap patch along with 'mapGetCached'.
mapCachePatch :: PatchOf (CacheMapOp k v) -> PatchOf (SetOp v)
mapCachePatch = fmap . fmap $ \(MkCachePair _ v) -> v
