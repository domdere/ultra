{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Control.Applicative
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Test.Ultra.Control.Applicative
--
-------------------------------------------------------------------
module Test.Ultra.Control.Applicative where

import Ultra.Control.Applicative

import Ultra.Control.Monad.Trans.Either (EitherT, left, runEitherT)

import qualified Data.List as L

import Lab.Core.Property
import Lab.Core.QuickCheck
import Lab.Core.QuickCheck.TH

import Preamble

prop_many1 :: NonNegative Int -> Property
prop_many1 (NonNegative n) = disjoin
    [   (n .>. 0) .&&. ((fst . flip runState n . runEitherT $ many1 producer) === pure (() :| L.replicate (n - 1) ()))
    ,   (n === 0) .&&. ((fst . flip runState n . runEitherT $ many1 producer) === Left ())
    ]

prop_checkPredicate_true :: (Arbitrary a, Show a, Eq a) => a -> Property
prop_checkPredicate_true = (===) <$> checkPredicate (const True) <*> Just

prop_checkPredicate_false :: (Arbitrary a, Show a, Eq a) => a -> Property
prop_checkPredicate_false x = checkPredicate (const False) x === Nothing

prop_guarded_true :: (Arbitrary a, Show a, Eq a) => a -> Property
prop_guarded_true = (===) <$> guarded True <*> Just

prop_guarded_false :: (Arbitrary a, Show a, Eq a) => a -> Property
prop_guarded_false x = guarded False x === Nothing

prop_guardPred_like_guard_true :: forall a. (Arbitrary a, Show a, Eq a) => a -> Property
prop_guardPred_like_guard_true x = guardPred (const True) x === (guarded True x :: Maybe a)

prop_guardPred_like_guard_false :: forall a. (Arbitrary a, Show a, Eq a) => a -> Property
prop_guardPred_like_guard_false x = guardPred (const False) x === (guarded False x :: Maybe a)

prop_guardMaybe :: (Arbitrary a, Show a, Eq a) => [a] -> Maybe a -> Property
prop_guardMaybe xs = (===) <$> guardMaybe xs <*> maybe xs pure

prop_liftMaybe :: (Arbitrary a, Show a, Eq a) => Maybe a -> Property
prop_liftMaybe = (===) <$> liftMaybe <*> maybe [] pure

-- helper

producer :: EitherT () (State Int) ()
producer = do
    x <- get
    unless (x > 0) (left ())
    put (x - 1)

return []
tests :: IO Bool
tests = $quickCheckAll
