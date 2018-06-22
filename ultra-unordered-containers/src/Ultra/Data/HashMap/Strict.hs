{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.HashMap.Strict
-- Copyright    : (C) 2018
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.HashMap.Strict (
  -- * Re-imports
    module X
  -- * Types
    , HashMapChange(..)
  -- * Functions
  , hashMapDiff2
  ) where

import Ultra.Data.Foldable (filteredBy)

import Data.HashMap.Strict as X
import Data.Hashable (Hashable)

import Preamble

data HashMapChange n h a =
    HashMapKeyUpdate !n !h !h !a -- ^ Old hash, new hash, new value
  | HashMapKeyNew !n !h !a -- ^ Hash, Value
  | HashMapKeyDelete !n -- ^ for keys that no longer exist
    deriving (Show, Eq)

-- This looks what what is effectively a 2 tier
-- hashmap (keyed first on @k@ and then on @n@
-- And detects the following kinds of differences:
--   - Values in the inner hashmaps that have changed
--   - Values in the inner hashmaps that have been deleted (i.e n keys being deleted)
--   - k keys being deleted.
--
-- It explicitly does *not* detect new k keys being added
-- The use case for this is syncing a bunch of things after they have been created.
-- So it doesnt handle new k's, something else does this, and this
-- Tracks changes to the @k@ keyed entities, including their removal.
hashMapDiff2
  :: forall k n a h. (Eq k, Eq h, Eq n, Hashable n, Hashable k)
  => (a -> h)
  -> [(k, HashMap n h)]
  -> [(k, HashMap n a)]
  -> [(k, NonEmpty (HashMapChange n h a))]
hashMapDiff2 hashFunction digests newValues =
  let
    newValuesAndHashes :: HashMap k (HashMap n (a, h))
    newValuesAndHashes =
      (fmap . fmap) ((,) <$> id <*> hashFunction) . fromList $ newValues

  in filteredBy (traverse nonEmpty) . flip fmap digests $ \(nm, digestStore') -> (,) nm $
    case lookup nm newValuesAndHashes of
      -- If the key doesnt show up in
      -- the new values,
      -- then the value must have been deleted
      Nothing ->
          fmap (HashMapKeyDelete . fst)
        . X.toList
        $ digestStore'
      Just newProjValsAndHashes ->
        let
          updatedValues :: [HashMapChange n h a]
          updatedValues =
            flip filteredBy (X.toList newProjValsAndHashes) $ \(varName, (newProjVal, newProjHash)) ->
              case lookup varName digestStore' of
                Nothing -> pure $ HashMapKeyNew varName newProjHash newProjVal
                Just oldHash ->
                  if oldHash == newProjHash then
                    Nothing
                  else
                    pure $ HashMapKeyUpdate
                      varName
                      oldHash
                      newProjHash
                      newProjVal

          deletedValues :: [HashMapChange n h a]
          deletedValues = flip filteredBy (fmap fst . X.toList $ digestStore') $ \varName ->
            case lookup varName newProjValsAndHashes of
              Nothing -> pure . HashMapKeyDelete $ varName
              Just _  -> Nothing
        in updatedValues <> deletedValues
