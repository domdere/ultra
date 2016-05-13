{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Data.Aeson
-- Copyright    : (C) 2016
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Data.Aeson where

import Ultra.Data.Aeson

import Lab.Core.QuickCheck
import Lab.Core.QuickCheck.TH (quickCheckAll)

import qualified Ultra.Data.Text as T

import Test.QuickCheck.Instances ()

import Preamble

prop_checkVersion_pass :: Int -> T.Text -> Property
prop_checkVersion_pass n v =
    let
        j :: Value
        j = toJSON n

        p :: (FromJSON a) => Value -> Parser a
        p v' = checkVersion v v (parseJSON v')
    in parseMaybe p j === pure n

prop_checkVersion_fail :: Int -> T.Text -> T.Text -> Property
prop_checkVersion_fail n v v' =
    let
        j :: Value
        j = toJSON n

        p :: (FromJSON a) => T.Text -> T.Text -> Value -> Parser a
        p v1 v2 val = checkVersion v1 v2 (parseJSON val)

        failString = T.unpack . T.concat $ [
                "Error in $: expected version '"
            ,   v'
            ,   "' but found '"
            ,   v
            ,   "'"
            ]
    in (v /= v') ==> (parseEither (p v v') j === (Left failString :: Either String Int))

return []
tests :: IO Bool
tests = $quickCheckAll
