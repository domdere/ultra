{-# LANGUAGE RankNTypes #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Control.Monad.Catch.Lens
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Control.Monad.Catch.Lens (
    -- * Functions
        isoVBracket
    ) where

import Ultra.Control.Lens (Iso', (^.), from)
import Ultra.Control.Monad.Bracket (VBracket, liftVBracket)

isoVBracket :: (forall a. Iso' (m a) (n a)) -> VBracket m -> VBracket n
isoVBracket i = liftVBracket (^. i) (^. from i)

--vbracketIso :: (forall a. Iso' (m a) (n a)) -> Iso' (VBracket m) (VBracket n)
--vbracketIso i = iso (isoVBracket i) (isoVBracket (from i))
