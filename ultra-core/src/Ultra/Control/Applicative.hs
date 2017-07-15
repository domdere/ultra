-------------------------------------------------------------------
-- |
-- Module       : Ultra.Control.Applicative
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Control.Applicative (
  -- * Functions
    many1
  , checkPredicate
  , guarded
  , guardPred
  , guardMaybe
  , liftMaybe
  ) where

import Preamble

many1 :: (Alternative f) => f a -> f (NonEmpty a)
many1 p = (:|) <$> p <*> many p

-- |
-- This function represents where i think everything goes wrong when people usually think
-- of applying predicates to filter etc.....
-- `Ultra.Data.Foldable.filteredBy` represents how i think filters should go, this
-- function will transform a predicate into the useless trivial @a -> `Maybe` a@ Kleisli
-- that causes `Ultra.Data.Foldable.filteredBy` to degenerate into `Data.List.filter`
--
-- It does bare similarity to `Control.Monad.guard`, and similar functionality
--
checkPredicate :: (Alternative f) => (a -> Bool) -> a -> f a
checkPredicate = (>>= guarded)


guarded :: (Alternative f) => Bool -> a -> f a
guarded p x = if p then pure x else empty

-- |
-- For translating conventional boolean based preds
-- to the @Maybe a@ kind that i use
--
guardPred :: (Alternative f) => (a -> Bool) -> a -> f a
guardPred = (>>= guarded)

-- |
-- Common use case for this is translating a `Preamble.Maybe` value into
-- something like an `Preamble.EitherT` value, or some other type where there is
-- no canonical "empty" value.
--
guardMaybe :: (Applicative f) => f a -> Maybe a -> f a
guardMaybe = flip maybe pure

-- |
-- like `guardMaybe` but when there is a canonical "empty" value.
--
liftMaybe :: (Alternative f) => Maybe a -> f a
liftMaybe = guardMaybe empty
