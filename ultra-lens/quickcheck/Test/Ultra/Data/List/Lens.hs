-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Data.List.Lens
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Data.List.Lens where

import Ultra.Data.List.Lens

import Lab.Control.Lens
import Lab.Core.QuickCheck
import Lab.Core.QuickCheck.TH

import Preamble

prop_lastElt :: (Show a, Eq a) => [a] -> Property
prop_lastElt = traversalLaws lastElt

prop_headElt :: (Show a, Eq a) => [a] -> Property
prop_headElt = traversalLaws headElt

return []
tests :: IO Bool
tests = $quickCheckAll
