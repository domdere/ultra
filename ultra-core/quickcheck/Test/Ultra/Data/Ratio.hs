{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Data.Ratio
-- Copyright    : (C) 2017
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Data.Ratio where

import Lab.Core.QuickCheck (Arbitrary(..), Property, (===), forAll, suchThat)
import Lab.Core.QuickCheck.TH (quickCheckAll)

import Ultra.Data.Ratio ((%?))

import Data.Ratio ((%))

import Preamble

prop_maybeRatio_zeroDenom :: Int -> Property
prop_maybeRatio_zeroDenom n = (n %? 0) === Nothing

prop_maybeRatio_nonZeroDenom :: Int -> Property
prop_maybeRatio_nonZeroDenom n = forAll (arbitrary `suchThat` (/= 0)) $ \d ->
  (n %? d) === pure (n % d)

return []
tests :: IO Bool
tests = $quickCheckAll
