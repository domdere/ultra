-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.Conduit.Text
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.Conduit.Text (
        module X
    -- * Functions
    ,   catchTextException
    ) where

import Ultra.Control.Monad.Trans.Either (EitherT, left)

import Data.Conduit.Text as X

import Preamble

catchTextException :: (Monad m) => (X.TextException -> e) -> TextException -> EitherT e m a
catchTextException f = left . f
