{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.Aeson
-- Copyright    : (C) 2016
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.Aeson (
      module X
  -- * Functions
  , checkVersion
  , jsonDispatchOnText
  , jsonTextEnum
  ) where

import qualified Ultra.Data.Text as T

import Data.Aeson as X
import Data.Aeson.Types as X

import Preamble

checkVersion :: T.Text -> T.Text -> Parser a -> Parser a
checkVersion v ev p =
  let
    failString = T.unpack . T.concat $ ["expected version '", ev, "' but found '", v, "'"]
  in unless (v == ev) (fail failString) >> p

jsonDispatchOnText :: NonEmpty (T.Text, Parser a) -> Value -> Parser a
jsonDispatchOnText cases v =
  let
    errorText :: T.Text -> T.Text
    errorText found = T.bracketedList "expected one of ['" (T.concat ["'], but found: '", found, "'"]) "', '" . toList . fmap fst $ cases
  in do
    t <- parseJSON v
    foldr
      (\(t', p') p -> if t == t' then p' else p)
      (fail . T.unpack . errorText $ t)
      cases

jsonTextEnum :: NonEmpty (T.Text, a) -> Value -> Parser a
jsonTextEnum = jsonDispatchOnText . fmap (fmap pure)
