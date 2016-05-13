{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Control.Monad.Trans.Either
-- Copyright    : (C) 2016
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Control.Monad.Trans.Either (
    -- * Types
        EitherT
    ,   pattern EitherT
    -- * Original EitherT Functions
    ,   runEitherT
    ,   eitherT
    ,   hoistEither
    ,   left
    ,   mapEitherT
    ,   bimapEitherT
    -- * Functions
    ,   unifyEitherT
    ) where

import Preamble

type EitherT = ExceptT

pattern EitherT m = ExceptT m

runEitherT :: EitherT e m a -> m (Either e a)
runEitherT = runExceptT

eitherT :: (Monad m) => (e -> m b) -> (a -> m b) -> EitherT e m a -> m b
eitherT g f = (either g f =<<) . runEitherT

left :: (Monad m) => e -> EitherT e m a
left = throwE

bimapEitherT :: (Functor m) => (e -> e') -> (a -> b) -> EitherT e m a -> EitherT e' m b
bimapEitherT l r = EitherT . fmap (bimap l r) . runEitherT

mapEitherT :: (m (Either e a) -> n (Either e' b)) -> EitherT e m a -> EitherT e' n b
mapEitherT f (ExceptT mx) = ExceptT $ f mx

hoistEither :: (Applicative f) => Either e a -> EitherT e f a
hoistEither = EitherT . pure

-- with parametericity, not much else this function can do other than what it does...
--
unifyEitherT :: (Monad m) => (e' -> e) -> EitherT e' (EitherT e m) a -> EitherT e m a
unifyEitherT f mx = runEitherT mx >>= hoistEither . first f


