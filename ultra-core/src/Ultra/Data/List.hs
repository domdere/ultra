-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.List
-- Copyright    : (C) 2015 - 2016
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.List (
    -- * Functions
        ordNub
    ,   stableOrdNub
    ) where

import qualified Data.List as X

import Preamble


ordNub :: (Ord a) => [a] -> [a]
ordNub = fmap head . group . X.sort

stableOrdNub :: (Ord a) => [a] -> [a]
stableOrdNub xs = fmap fst . X.sortBy (compare `on` snd) . fmap head . groupBy ((==) `on` fst) . X.sort $ X.zip xs ([0..] :: [Int])

