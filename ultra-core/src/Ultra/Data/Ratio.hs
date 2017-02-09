{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.Ratio
-- Copyright    : (C) 2017
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.Ratio (
  -- * Operators
    (%!)
  , (%?)
  ) where

import Ultra.Data.NonZero (
    NonZero
  , getNonZero
  , nonZero
  )

import Data.Ratio (Ratio, (%))

import Preamble

infixl 7 %!
infixl 7 %?

(%!) :: (Integral a) => a -> NonZero a -> Ratio a
num %! denom = num % (getNonZero denom)

(%?) :: (Integral a) => a -> a -> Maybe (Ratio a)
num %? denom = (num %!) <$> nonZero denom
