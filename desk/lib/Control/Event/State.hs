{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Event.State (
  Discrete,
  exeMapAccum,
  exeAccumD,
  ZipToRight (..),
  zipToRightM,
  Act (..),
  Patch (..),
  ColPatchEl (..),
  ColPatch (..),
  applyImpure,
  CacheVec (..),
  vecGetCached,
  CacheMap (..),
  mapGetCached,
  mapPatchCached,
  withOnRemove,
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

data ColPatchEl a = AddEl !a | RemoveEl !a
  deriving (Eq, Show, Functor)

-- | Patch for collections. This is to apply collection patches onto the view.
--
-- Patch is applied from left to right.
newtype ColPatch a = ColPatch (V.Vector (ColPatchEl a))
  deriving (Eq, Show)

-- TODO Consider adding Monoid

applyPatches :: (ColPatchEl a -> m -> m) -> (ColPatch a -> m -> m)
applyPatches apply (ColPatch ops) x = foldl' (flip apply) x ops

applyImpure :: Applicative m => (ColPatchEl a -> m ()) -> (ColPatch a -> m ())
applyImpure asAct (ColPatch x) = traverse_ asAct x

instance Eq a => Act (ColPatch a) (V.Vector a) where
  (<:) :: Eq a => ColPatch a -> V.Vector a -> V.Vector a
  (<:) = applyPatches $ \case
    AddEl x -> (`V.snoc` x)
    RemoveEl x -> V.filter (/= x)

instance Ord a => Act (ColPatch a) (S.Set a) where
  (<:) :: Ord a => ColPatch a -> S.Set a -> S.Set a
  (<:) = applyPatches $ \case
    AddEl x -> S.insert x
    RemoveEl x -> S.delete x

-- | Cache vector, where each element is uniquely determined by position.
--
-- Action on cache vector only operates from the right side.
newtype CacheVec a = AsCacheVec (V.Vector a)
  deriving (Eq, Show)

instance Act (ColPatch a) (CacheVec a) where
  (<:) :: ColPatch a -> CacheVec a -> CacheVec a
  (<:) = applyPatches $ (coerce .) $ \case
    AddEl x -> (`V.snoc` x)
    RemoveEl _ -> V.init

instance Patch (ColPatch a) (CacheVec a) where
  (<--) :: CacheVec a -> CacheVec a -> ColPatch a
  AsCacheVec new <-- AsCacheVec old = ColPatch $
    case V.length old `compare` V.length new of
      LT -> AddEl <$> vecSubtract new old
      EQ -> V.empty
      -- Reverses because it removes from back to front
      GT -> RemoveEl <$> V.reverse (vecSubtract old new)
    where
      vecSubtract v w = V.drop (V.length w) v

-- | Gets the cached.
vecGetCached :: CacheVec a -> V.Vector a
vecGetCached = coerce

-- | Cache map, where each element is uniquely determined by position.
newtype CacheMap k v = AsCacheMap (M.Map k v)
  deriving (Eq, Show)

-- Reuses ColPatch simply for saving amount of code.
instance Ord k => Act (ColPatch (k, v)) (CacheMap k v) where
  (<:) :: ColPatch (k, v) -> CacheMap k v -> CacheMap k v
  (<:) =
    applyPatches $
      coerce . \case
        AddEl (k, v) -> M.insert k v
        RemoveEl (k, _) -> M.delete k

instance Ord k => Patch (ColPatch (k, v)) (CacheMap k v) where
  (<--) :: Ord k => CacheMap k v -> CacheMap k v -> ColPatch (k, v)
  AsCacheMap new <-- AsCacheMap old = ColPatch . V.fromList $ (AddEl <$> inserted) <> (RemoveEl <$> deleted)
    where
      inserted = M.toList $ new M.\\ old
      deleted = M.toList $ old M.\\ new

-- Bag should be more appropriate, but it requires more implementations.
mapGetCached :: Ord v => CacheMap k v -> S.Set v
mapGetCached = S.fromList . M.elems . coerce

mapPatchCached :: ColPatch (k, v) -> ColPatch v
mapPatchCached (ColPatch ops) = ColPatch (V.map (fmap snd) ops)

-- | Attach removal action to update function.
withOnRemove ::
  (Monad m, Patch (ColPatch a) s) => (t -> s) -> (a -> m ()) -> (t -> m t) -> (t -> m t)
-- TODO This is kind of a hack, look for a better way.
withOnRemove asPatched delete update old = do
  new <- update old
  new <$ applyImpure actOn (asPatched new <-- asPatched old)
  where
    actOn (AddEl _) = pure ()
    actOn (RemoveEl x) = delete x
