{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Data.RealFrac
-- Copyright    : (C) 2017
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Data.RealFrac where

import Ultra.Data.RealFrac

import Lab.Core.QuickCheck (NonNegative(..), Positive(..), Property, (===))
import Lab.Core.QuickCheck.TH (quickCheckAll)

import Data.Ratio (Ratio)

import Preamble

prop_ceilingMagPos :: NonNegative (Ratio Int) -> Property
prop_ceilingMagPos (NonNegative x) =
  ceilingMag x === (ceiling x :: Int)

prop_ceilingMagNeg :: Positive (Ratio Int) -> Property
prop_ceilingMagNeg (Positive x) =
  ceilingMag (-x) === (floor (-x) :: Int)

prop_floorMagPos :: NonNegative (Ratio Int) -> Property
prop_floorMagPos (NonNegative x) =
  floorMag x === (floor x :: Int)

prop_floorMagNeg :: Positive (Ratio Int) -> Property
prop_floorMagNeg (Positive x) =
  floorMag (-x) === (ceiling (-x) :: Int)

return []
tests :: IO Bool
tests = $quickCheckAll
