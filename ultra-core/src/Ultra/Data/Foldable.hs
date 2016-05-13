-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.Foldable
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.Foldable (
    -- * Functions
        filteredBy
    ,   head
    ) where

import Preamble hiding ( head )

filteredBy :: (Foldable f) => (a -> Maybe b) -> f a -> [b]
filteredBy f = foldr (maybe id (:) . f) []

head :: (Foldable f) => f a -> Maybe a
head = foldr (\x _ -> pure x) Nothing
