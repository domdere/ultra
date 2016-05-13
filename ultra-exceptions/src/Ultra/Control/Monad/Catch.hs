{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Control.Monad.Catch
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Control.Monad.Catch (
        module X
    -- Functions
    ,   maskedBracket
    ) where

import Control.Monad.Catch as X

import Preamble

maskedBracket
    :: (MonadMask m)
    => m a
    -> (Maybe b -> a -> m c)
    -> (a -> m b)
    -> m (c, b)
maskedBracket acquire release use = mask $ \unmasked -> do
    resource <- acquire
    result <- unmasked (use resource) `onException` release Nothing resource
    releaseResult <- release (pure result) resource
    pure (releaseResult, result)
