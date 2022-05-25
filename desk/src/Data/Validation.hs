module Data.Validation where

import Control.Applicative

-- | Validation applicative, which appends all errors together.
-- Copied from Data.Either.Validation
data Validation e a = Failure e | Success a

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure e1 <*> Success _ = Failure e1
  Success _ <*> Failure e2 = Failure e2
  Success f <*> Success a = Success (f a)

instance Monoid e => Alternative (Validation e) where
  empty = Failure mempty
  Success a <|> _ = Success a
  _ <|> Success b = Success b
  Failure e1 <|> Failure e2 = Failure (e1 <> e2)

validation :: (e -> c) -> (a -> c) -> Validation e a -> c
validation l r = \case
  Failure e -> l e
  Success a -> r a

validToEither :: Validation e a -> Either e a
validToEither = validation Left Right

eitherToValid :: Either e a -> Validation e a
eitherToValid = either Failure Success
