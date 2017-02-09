{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Data.NonZero
-- Copyright    : (C) 2017
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Data.NonZero where

import Lab.Core.Property ((=/=))
import Lab.Core.QuickCheck (
    Arbitrary(..)
  , Gen
  , Property
  , (===)
  , conjoin
  , forAll
  , suchThat
  )
import Lab.Core.QuickCheck.TH (quickCheckAll)

import Ultra.Data.NonZero (
    NonZero
  , nonZero
  , getNonZero
  )

import Preamble

prop_nonZero_zero :: Property
prop_nonZero_zero = nonZero (0 :: Int) === Nothing

prop_nonZero_notZero :: Property
prop_nonZero_notZero = forAll (arbitrary `suchThat` (/= 0) :: Gen Int) $ \x ->
  let
    n :: Maybe (NonZero Int)
    n = nonZero x
  in conjoin [
      n =/= Nothing
    , (getNonZero <$> n) === pure x
    
    ]

return []
tests :: IO Bool
tests = $quickCheckAll
