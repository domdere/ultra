{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.RealFrac
-- Copyright    : (C) 2017
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.RealFrac (
  -- * Function
    ceilingMag
  , floorMag
  ) where

import Preamble

ceilingMag :: (Integral a, RealFrac b) => b -> a
ceilingMag r
  | r < 0     = floor r
  | otherwise = ceiling r

floorMag :: (Integral a, RealFrac b) => b -> a
floorMag r
  | r < 0     = ceiling r
  | otherwise = floor r
