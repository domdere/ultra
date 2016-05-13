-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Data.Foldable
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Data.Foldable ( tests ) where

import Ultra.Data.Foldable

import Test.QuickCheck ( Arbitrary, Property, (===), quickCheckAll )

import Preamble hiding ( head )

prop_filteredBy :: [Int] -> Property
prop_filteredBy xs =
    let
        f :: Int -> Bool
        f x
            |   x < 15      = False
            |   otherwise   = True
    in filteredBy (\x -> if f x then pure x else Nothing) xs === filter f xs

prop_head_nonempty_list :: (Arbitrary a, Show a, Eq a) => a -> [a] -> Property
prop_head_nonempty_list x xs = head (x:xs) === pure x

prop_head_empty_list :: Property
prop_head_empty_list = head [] === (Nothing :: Maybe Int)

prop_head_maybe :: (Arbitrary a, Show a, Eq a) => Maybe a -> Property
prop_head_maybe = (===) <$> head <*> id

prop_head_either :: (Arbitrary b, Arbitrary a, Show a, Show b, Eq a, Eq b) => Either a b -> Property
prop_head_either = (===) <$> head <*> either (const Nothing) pure

return []
tests :: IO Bool
tests = $quickCheckAll
