{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.NonZero
-- Copyright    : (C) 2017
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.NonZero (
  -- * Types
    NonZero
  -- * Functions
  , nonZero
  , getNonZero
  ) where

import Preamble

newtype NonZero a = NonZero a
  deriving (Show, Eq)

nonZero
  :: (Eq a, Integral a)
  => a
  -> Maybe (NonZero a)
nonZero 0 = Nothing
nonZero n = pure $ NonZero n

getNonZero :: NonZero a -> a
getNonZero (NonZero x) = x

