{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Data.Either
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Data.Either ( tests ) where

import Ultra.Data.Either

import Test.QuickCheck ( Arbitrary, Property, (===), quickCheckAll )

import Preamble

prop_guardEither_true
    :: (Arbitrary a, Arbitrary e, Show a, Show e, Eq a, Eq e)
    => e -> a -> Property
prop_guardEither_true err = (===) <$> guardEither err (const True) <*> pure

prop_guardEither_false
    :: (Arbitrary a, Arbitrary e, Show a, Show e, Eq a, Eq e)
    => e -> a -> Property
prop_guardEither_false err x = guardEither err (const False) x === Left err

prop_filterEither_pure
    :: (Arbitrary a, Arbitrary e, Show a, Show e, Eq a, Eq e)
    => e -> a -> Property
prop_filterEither_pure err = (===) <$> filterEither err pure <*> pure

prop_filterEither_Nothing
    :: forall a e. (Arbitrary a, Arbitrary e, Show a, Show e, Eq a, Eq e)
    => e -> a -> Property
prop_filterEither_Nothing err x = filterEither err (const (Nothing :: Maybe a)) x === Left err


return []
tests :: IO Bool
tests = $quickCheckAll
