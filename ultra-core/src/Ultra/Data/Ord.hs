{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.Ord
-- Copyright    : (C) 2017
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.Ord (
  -- * Functions
    maxOn
  , minOn
  ) where

import Preamble

maxOn :: (Ord b) => (a -> b) -> a -> a -> a
maxOn f x y
  | f x < f y = y
  | otherwise = x

minOn :: (Ord b) => (a -> b) -> a -> a -> a
minOn f x y
  | f x > f y = y
  | otherwise = x
