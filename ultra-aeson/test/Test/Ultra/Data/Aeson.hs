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

import Lab.Core.Gen (distinctListOfN1, textOf1, alphaNumChars)
import Lab.Core.QuickCheck (Arbitrary(..), Gen, Property, (===), (==>), choose, elements, forAll, suchThat)
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

prop_jsonTextEnum_success :: Property
prop_jsonTextEnum_success =
  let
    tags :: Gen (NonEmpty (T.Text, Int), T.Text, Int)
    tags = do
      listSize <- fromIntegral <$> choose (0, 39 :: Int)
      ts <- distinctListOfN1 listSize (textOf1 alphaNumChars)
      ts' <- forM ts $ \x -> (,) x <$> arbitrary
      (testTag, expectedResult) <- elements . toList $ ts'
      pure (ts', testTag, expectedResult)
  in forAll tags $ \(tags', tagToCheck, expected) ->
    parseEither (jsonTextEnum tags') (toJSON tagToCheck) === pure expected

prop_jsonTextEnum_fail :: Property
prop_jsonTextEnum_fail =
  let
    tags :: Gen (NonEmpty (T.Text, Int), T.Text)
    tags = do
      listSize <- fromIntegral <$> choose (0, 39 :: Int)
      testTag <- textOf1 alphaNumChars
      ts <- distinctListOfN1 listSize ((textOf1 alphaNumChars) `suchThat` (/= testTag))
      ts' <- forM ts $ \x -> (,) x <$> arbitrary
      pure (ts', testTag)
  in forAll tags $ \(tags', tagToCheck) ->
    parseMaybe (jsonTextEnum tags') (toJSON tagToCheck) === Nothing

return []
tests :: IO Bool
tests = $quickCheckAll
