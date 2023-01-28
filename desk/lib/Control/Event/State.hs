{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Event.State (
  Discrete,
  exeMapAccum,
  exeAccumD,
  ZipToRight (..),
  FilterZipToRight (..),
) where

import Data.Map.Merge.Strict qualified as M
import Data.Map.Strict qualified as M
import Data.Vector qualified as V
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

-- | Discrete behavior paired with its own update event.
type Discrete a = (Event a, Behavior a)

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

class (Functor t, Foldable t) => ZipToRight t where
  zipToRightM :: Monad m => (Maybe a -> b -> m c) -> t a -> t b -> m (t c)

instance ZipToRight V.Vector where
  zipToRightM :: Monad m => (Maybe a -> b -> m c) -> V.Vector a -> V.Vector b -> m (V.Vector c)
  zipToRightM fn left = V.imapM $ \idx -> fn (left V.!? idx)

class (Functor t, Foldable t) => FilterZipToRight t where
  filterZipToRightM :: Monad m => (Maybe a -> b -> m (Maybe c)) -> t a -> t b -> m (t c)

instance Ord k => FilterZipToRight (M.Map k) where
  filterZipToRightM :: (Ord k, Monad m) => (Maybe a -> b -> m (Maybe c)) -> M.Map k a -> M.Map k b -> m (M.Map k c)
  filterZipToRightM fn = M.mergeA M.dropMissing onlyRight both
    where
      onlyRight = M.traverseMaybeMissing $ \_key -> fn Nothing
      both = M.zipWithMaybeAMatched $ \_key l -> fn (Just l)
