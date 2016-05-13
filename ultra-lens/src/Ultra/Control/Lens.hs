{-# LANGUAGE RankNTypes #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Control.Lens
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Control.Lens (
        module X
    -- * Operators
    ,   (%=!)
    ,   (%=!!)
    -- * Setters
    ,   leftT
    -- * Traversals
    ,   selected
    ) where

import Ultra.Control.Monad.Trans.Either (EitherT, bimapEitherT)

import Control.Lens as X

import Preamble

infix 4 %=!
infix 4 %=!!

-- |
-- Theres probably some thing like this in `Control.Lens` but I couldn't find it.
-- The intention is to allow strict eval (well at least hopefully whnf is enough)
-- of state modifications as we go, as I have found that combining `StateT`
-- with streaming libraries like `conduit` can result in memory leaks
-- when it comes to accumulated state that isnt forced until the streaming has completed..
--
(%=!) :: (MonadState s m) => ASetter s s a b -> (a -> b) -> m ()
l %=! f = get >>= (put $!) . over l f


-- |
-- Again couldnt find this one anywhere in `Control.Lens`,
-- `%=!` only evaluates the overall state type to WHNF,
-- while `%=!!` also ealuates the record that its setting to WHNF also.
-- Usually I suppose people would instead handle this in the data type by
-- putting a bang on that arg of the Constructor, this is here for the cases where
-- you might be reusing a type that otherwise has no reason for the bang in it.
--
(%=!!) :: (MonadState s m) => Setting (->) s s a b -> (a -> b) -> m ()
l %=!! f = get >>= (put $!) . over l (f $!)

-- setters

leftT :: (Functor m) => Setter (EitherT e m a) (EitherT e' m a) e e'
leftT = sets $ flip bimapEitherT id

-- traversals

selected :: (a -> Bool) -> Traversal' (a, b) b
selected p f (x, y) = if p x then (,) x <$> f y else pure (x, y)
