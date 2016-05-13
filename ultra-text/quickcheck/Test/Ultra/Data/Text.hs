{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Data.Text
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Data.Text where

import qualified Ultra.Data.Text as T

import Test.QuickCheck ( Property, (===), conjoin, quickCheckAll )
import Test.QuickCheck.Instances ()

import Preamble

prop_bracketed :: T.Text -> T.Text -> T.Text -> Property
prop_bracketed open close body = T.bracketed open close body === (open <> body <> close)

prop_bracketedList :: T.Text -> T.Text -> T.Text -> [T.Text] -> Property
prop_bracketedList open close sepBy body =
    T.bracketedList open close sepBy body === (open <> (T.intercalate sepBy body) <> close)

return []
tests :: IO Bool
tests = $quickCheckAll
