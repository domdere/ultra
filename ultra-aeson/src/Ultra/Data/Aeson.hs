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
    ,   checkVersion
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
