-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.Either
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.Either (
    -- * Functions
        filterEither
    ,   guardEither
    ) where

import Preamble

-- |
-- This is better than the `Control.Monad.guard` that comes
-- with `Control.Monad.MonadPlus`
--
guardEither :: e -> (a -> Bool) -> a -> Either e a
guardEither err p x
    | p x       = pure x
    | otherwise = Left err

filterEither :: e -> (a -> Maybe b) -> a -> Either e b
filterEither err f = maybe (Left err) pure . f
