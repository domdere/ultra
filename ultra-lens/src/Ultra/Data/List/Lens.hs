-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.List.Lens
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.List.Lens (
    -- * Re-exports
        module X
    -- * Traversals
    ,   headElt
    ,   lastElt
    ) where

import Control.Lens ( Traversal' )
import Data.List.Lens as X

import Preamble

lastElt :: Traversal' [a] a
lastElt _ []        = pure []
lastElt f [x]       = pure <$> f x
lastElt f (x:xs)    = (x:) <$> lastElt f xs

headElt :: Traversal' [a] a
headElt _ []        = pure []
headElt f (x:xs)    = (:xs) <$> f x
