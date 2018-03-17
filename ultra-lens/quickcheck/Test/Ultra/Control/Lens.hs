{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Control.Lens
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Control.Lens where

import Ultra.Control.Lens
import Ultra.Control.Monad.Trans.Either (EitherT, left)

import Lab.Control.Lens

import Lab.Core.QuickCheck
import Lab.Core.QuickCheck.TH

import Preamble
import Prelude ( mod )

prop_selected :: (Arbitrary a, Show a, Eq a) => (Int, a) -> Property
prop_selected = traversalLaws $ selected ((== 0) . (`mod` 2))

prop_leftT :: Int -> Int -> Property
prop_leftT y y' =
    let
        lefts :: Gen (EitherT Int Identity a)
        lefts = left <$> arbitrary

        rights :: Gen (EitherT e Identity Int)
        rights = pure <$> arbitrary

        eitherTs :: Gen (EitherT Int Identity Int)
        eitherTs = oneof [lefts, rights]
    in forAll eitherTs $ \x -> setterLaws leftT x y y'

return []
tests :: IO Bool
tests = $quickCheckAll
