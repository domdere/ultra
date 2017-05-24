{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Data.List.NonEmpty
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Data.List.NonEmpty ( tests ) where

import Ultra.Data.List.NonEmpty

import Test.QuickCheck ( Arbitrary, Property, (===), quickCheckAll )

import Preamble

prop_groupBy2_groupBy :: (Arbitrary a, Show a, Eq a) => [a] -> Property
prop_groupBy2_groupBy xs =
    let
        diag :: a -> (a, a)
        diag = (,) <$> id <*> id
    in (snd <$> groupBy2 diag xs) === groupBy (==) xs

return []
tests :: IO Bool
tests = $quickCheckAll
