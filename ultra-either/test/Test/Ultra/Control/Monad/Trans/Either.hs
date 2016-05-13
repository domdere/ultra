{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Control.Monad.Trans.Either
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Control.Monad.Trans.Either where

import Lab.Core.QuickCheck.TH

import Preamble

return []
tests :: IO Bool
tests = $quickCheckAll
