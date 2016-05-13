-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Data.Maybe
-- Copyright    : (C) 2014
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Data.Maybe ( tests ) where

import Ultra.Data.Maybe

import Test.QuickCheck ( Arbitrary, Property, (===), quickCheckAll )

import Preamble

return []
tests :: IO Bool
tests = $quickCheckAll
