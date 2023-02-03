{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Event.State (
  Discrete,
  exeMapAccum,
  exeAccumD,
  ZipToRight (..),
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
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

-- | mapAccum which accumulates by executing momentous action.
exeMapAccum :: acc -> Event (acc -> MomentIO (sig, acc)) -> MomentIO (Event sig, Behavior acc)
exeMapAccum initial eFn = do
  rec bAcc <- stepper initial eAcc
      eSigAcc <- execute (newSigAcc <$> bAcc <@> eFn)
      let eSig = fst <$> eSigAcc
          eAcc = snd <$> eSigAcc
  pure (eSig, bAcc)
  where
    newSigAcc acc update = acc `seq` update acc

-- | Discrete behavior which accumulates by executing momentous action.
exeAccumD :: a -> Event (a -> MomentIO a) -> MomentIO (Discrete a)
exeAccumD initial eFn = do
  exeMapAccum initial (withSig <$> eFn)
  where
    both x = (x, x)
    withSig fn = fmap both . fn

-- TODO Attach delete action?

-- Pads left list to fit right. (In contrast, SemiAlign acts like union)
class (Functor t, Foldable t) => ZipToRight t where
  zipToRightM :: Monad m => (Maybe a -> b -> m c) -> t a -> t b -> m (t c)

instance ZipToRight V.Vector where
  zipToRightM :: Monad m => (Maybe a -> b -> m c) -> V.Vector a -> V.Vector b -> m (V.Vector c)
  zipToRightM fn left = V.imapM $ \idx -> fn (left V.!? idx)

instance Ord k => ZipToRight (M.Map k) where
  zipToRightM :: (Ord k, Monad m) => (Maybe a -> b -> m c) -> M.Map k a -> M.Map k b -> m (M.Map k c)
  zipToRightM fn = M.mergeA M.dropMissing onlyRight both
    where
      onlyRight = M.traverseMissing $ \_key -> fn Nothing
      both = M.zipWithAMatched $ \_key l -> fn (Just l)

-- | Difference of a container, represented by pair of removed and added stuffs.
data Diffs a = Diffs {removed :: a, added :: a}
  deriving (Show, Functor)

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
-- When parameters are @x@ and @y@, this requires @x /\ y = y /\ x@.
computeDiffs :: SetLike m => m -> m -> Diffs m
computeDiffs old new = Diffs{removed = old \\ new, added = new \\ old}
