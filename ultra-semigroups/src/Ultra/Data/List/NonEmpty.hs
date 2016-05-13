{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.List.NonEmpty
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.List.NonEmpty (
    -- * Re imports
        module X
    -- * Functions
    ,   groupBy2
    ) where

import Data.List.NonEmpty as X

import Preamble

-- |
-- like `groupBy` but transforms and groups.
--
-- The input function can be thought of as splitting out the `a` type into two parts, the parts that are common and the parts that are distinct.
--
groupBy2 :: forall k f a b. (Eq k, Foldable f) => (a -> (k, b)) -> f a -> [(k, NonEmpty b)]
groupBy2 f =
    let
        groupBy2' :: a -> [(k, NonEmpty b)] -> [(k, NonEmpty b)]
        groupBy2' x []                  = (\(u, y) -> pure (u, pure y)) . f $ x
        groupBy2' x zs@((v, ys'):zs')   = (\(u, y) -> if u == v then (v, y <| ys'):zs' else (u, pure y) : zs) . f $ x
    in foldr groupBy2' []
