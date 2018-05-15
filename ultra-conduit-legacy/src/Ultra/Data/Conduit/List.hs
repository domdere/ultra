{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.Conduit.List
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.Conduit.List (
    -- * Re-imports
        module X
    -- * Conduits
    ,   foldWithPrevious
    ) where

import qualified Data.Conduit as C
import Data.Conduit.List as X

import Preamble

-- | This produces a combined stream of i's and t's where
-- the values of the ts depend ont he current value of the i and the previous
-- value of the t if there is one (hence @Maybe t@)
--
foldWithPrevious
    :: forall m i t. (Monad m)
    => (i -> Maybe t -> m t)
    -> C.Conduit i m (i, t)
foldWithPrevious f =
    let
        doStuff :: (Monad m) => (i -> Maybe t -> m t) -> Maybe t -> C.Conduit i m (i, t)
        doStuff f' my = do
            mx <- C.await
            forM_ mx $ \x -> do
                y <- lift $ f' x my
                C.yield (x, y)
                doStuff f' (pure y)
    in doStuff f Nothing
