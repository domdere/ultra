{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Data.Ord
-- Copyright    : (C) 2017
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Data.Ord where

import Ultra.Data.Ord

import Lab.Core.QuickCheck (Property, (===))
import Lab.Core.QuickCheck.TH (quickCheckAll)

import Preamble

prop_minOn :: Int -> Int -> Property
prop_minOn x y =
  minOn id x y === min x y

prop_maxOn :: Int -> Int -> Property
prop_maxOn x y =
  maxOn id x y === max x y

return []
tests :: IO Bool
tests = $quickCheckAll
