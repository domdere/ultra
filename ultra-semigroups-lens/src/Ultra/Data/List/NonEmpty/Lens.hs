-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.List.NonEmpty.Lens
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.List.NonEmpty.Lens (
    -- * Lenses
        headEltNE
    ,   lastEltNE
    ) where

import Ultra.Control.Lens ( Lens', (.~), lens )
import Ultra.Data.List.Lens
import qualified Ultra.Data.List.NonEmpty as NE

import Preamble

lastEltNE :: Lens' (NonEmpty a) a
lastEltNE =
    let
        set' :: NonEmpty a -> a -> NonEmpty a
        set' (_ :| []) y = y :| []
        set' (x :| xs) y = x :| (lastElt .~ y) xs
    in lens NE.last set'

headEltNE :: Lens' (NonEmpty a) a
headEltNE = lens head $ \(_ :| xs) x -> x :| xs
