-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Data.List.NonEmpty.Lens
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Data.List.NonEmpty.Lens where

import Ultra.Data.List.NonEmpty.Lens

import Lab.Control.Lens
import Lab.Core.Gen
import Lab.Core.QuickCheck
import Lab.Core.QuickCheck.TH

import Preamble

prop_headEltNE :: Property
prop_headEltNE = forAll (listOf1 ints) $ lensLaws headEltNE

prop_lastEltNE :: Property
prop_lastEltNE = forAll (listOf1 ints) $ lensLaws lastEltNE

-- helpers

ints :: Gen Int
ints = arbitrary

return []
tests :: IO Bool
tests = $quickCheckAll
