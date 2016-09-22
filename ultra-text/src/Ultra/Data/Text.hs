{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.Text
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Ultra text functions
--
-------------------------------------------------------------------
module Ultra.Data.Text (
    module T
    -- * Functions
    ,   bracketed
    ,   bracketedList
    ) where

import Data.Text as T

import Preamble

bracketed :: Text -> Text -> Text -> Text
bracketed open close t = T.concat [open, t, close]

bracketedList :: Text -> Text -> Text -> [Text] -> Text
bracketedList open close sepBy = bracketed open close . intercalate sepBy
