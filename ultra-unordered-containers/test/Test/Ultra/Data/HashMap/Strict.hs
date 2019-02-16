{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Ultra.Data.HashMap.Strict where

import qualified Ultra.Data.HashMap.Strict as H
import qualified Ultra.Data.HashSet as S

import Lab.Core.Hedgehog
import qualified Lab.Core.Hedgehog.Gen as Gen
import qualified Lab.Core.Hedgehog.Range as Range
import Lab.Core.Hedgehog.TH

import Data.Hashable (Hashable)

import Preamble

prop_hashMapDiff_update :: Property
prop_hashMapDiff_update = property $ do
  keys <- forAll $ Gen.nonEmpty
    (Range.constant 1 100)
    (Gen.int $ Range.constant 0 2000)
  distinctKeys <- case nonEmpty . S.toList . S.fromList . toList $ keys of
    Nothing -> annotate "Got empty list of distinct keys from non empty list of keys (impossible)" >> failure
    Just x -> pure x
  oldValues <- forM distinctKeys $ \key -> do
    value <- forAll $ Gen.int (Range.constantFrom 0 (-1000) 1000)
    pure (key, value)
  -- pick a key to update
  (key, oldVal) <- forAll . Gen.element $ oldValues
  newVal <- forAll $ (/= oldVal) `Gen.filter` Gen.int (Range.constantFrom 0 (-1000) 1000)
  let
    oldHashMap :: H.HashMap Int Int
    oldHashMap =
      H.fromList . toList $ oldValues

    newHashMap :: H.HashMap Int Int
    newHashMap =
      H.insert key newVal oldHashMap

    expectedResult :: [H.HashMapChange Int Int Int]
    expectedResult = pure $
      H.HashMapKeyUpdate key oldVal newVal newVal
  -- The type ensures that the hash function is used
  -- to extract info from the input hash map of hashes,
  -- So we can simplify the test by testing with the
  -- identity hash function.
  H.hashMapDiff id oldHashMap newHashMap === expectedResult

prop_hashMapDiff_new :: Property
prop_hashMapDiff_new = property $ do
  keys <- forAll $ Gen.nonEmpty
    (Range.constant 1 100)
    (Gen.int $ Range.constant 0 2000)
  distinctKeys <- case nonEmpty . S.toList . S.fromList . toList $ keys of
    Nothing -> annotate "Got empty list of distinct keys from non empty list of keys (impossible)" >> failure
    Just x -> pure x
  oldValues <- forM distinctKeys $ \key -> do
    value <- forAll $ Gen.int (Range.constantFrom 0 (-1000) 1000)
    pure (key, value)
  -- The other keys were chosen from the range 0-2000 so this should be guaranteed
  -- to be new to the map...
  newKey <- forAll $ Gen.int (Range.constantFrom 2500 2000 3000)
  newValue <- forAll $ Gen.int (Range.constantFrom 0 (-1000) 1000)
  let
    oldHashMap =
      H.fromList . toList $ oldValues
    newHashMap =
      H.insert newKey newValue oldHashMap

    expectedResult = pure $
      H.HashMapKeyNew newKey newValue newValue
  -- The type ensures that the hash function is used
  -- to extract info from the input hash map of hashes,
  -- So we can simplify the test by testing with the
  -- identity hash function.
  H.hashMapDiff id oldHashMap newHashMap === expectedResult

prop_hashMapDiff_delete :: Property
prop_hashMapDiff_delete = property $ do
  keys <- forAll $ Gen.nonEmpty
    (Range.constant 1 100)
    (Gen.int $ Range.constant 0 2000)
  distinctKeys <- case nonEmpty . S.toList . S.fromList . toList $ keys of
    Nothing -> annotate "Got empty list of distinct keys from non empty list of keys (impossible)" >> failure
    Just x -> pure x
  oldValues <- forM distinctKeys $ \key -> do
    value <- forAll $ Gen.int (Range.constantFrom 0 (-1000) 1000)
    pure (key, value)
  -- pick a key to delete
  (key, _) <- forAll . Gen.element $ oldValues
  let
    oldHashMap = H.fromList . toList $ oldValues

    newHashMap = H.delete key oldHashMap

    expectedResult = pure $
      H.HashMapKeyDelete key
  -- The type ensures that the hash function is used
  -- to extract info from the input hash map of hashes,
  -- So we can simplify the test by testing with the
  -- identity hash function.
  H.hashMapDiff id oldHashMap newHashMap === expectedResult


prop_hashMapDiff2_update :: Property
prop_hashMapDiff2_update = property $ do
  -- pick a key to update
  (key, map2, oldValues) <- hashMapDiff2KeysAndMaps 
  let oldHashMap = H.fromList . toList $ oldValues
  map2Entries <- case nonEmpty . H.toList $ map2 of
    Nothing -> annotate "second level map is empty..." >> failure
    Just x -> pure x
  (key2, oldVal) <- forAll . Gen.element $ map2Entries
  newHashMap <- flip H.traverseWithKey oldHashMap $ \k map2' ->
    case key == k of
      False -> pure map2'
      True ->
        flip H.traverseWithKey map2' $ \k2 v ->
          case key2 == k2 of
            False -> pure v
            True -> forAll $
              (/= v) `Gen.filter` Gen.int (Range.constantFrom 0 (-1000) 1000)
  newVal <- case H.lookup key newHashMap >>= H.lookup key2 of
    Nothing -> annotate "Could not find new value" >> failure
    Just v -> pure v
  let
    expectedResult = pure
      (key, pure $ H.HashMapKeyUpdate key2 oldVal newVal newVal)
  -- The type ensures that the hash function is used
  -- to extract info from the input hash map of hashes,
  -- So we can simplify the test by testing with the
  -- identity hash function.
  H.hashMapDiff2 id (toList oldValues) (H.toList newHashMap) === expectedResult

hashMapDiff2KeysAndMaps
  :: PropertyT IO (Int, H.HashMap Int Int, NonEmpty (Int, H.HashMap Int Int))
hashMapDiff2KeysAndMaps = do
  keys <- forAll $ Gen.nonEmpty
    (Range.constant 1 100)
    (Gen.int $ Range.constant 0 2000)
  distinctKeys <- case nonEmpty . S.toList . S.fromList . toList $ keys of
    Nothing -> annotate "Got empty list of distinct keys from non empty list of keys (impossible)" >> failure
    Just x -> pure x
  oldValues <- forM distinctKeys $ \key -> do
    keys2 <- forAll $ Gen.nonEmpty
      (Range.constant 1 100)
      (Gen.int $ Range.constant 0 2000)
    let distinctKeys2 = S.toList . S.fromList . toList $ keys2
    subMapList <-forM distinctKeys2 $ \key2 -> do
      value <- forAll $ Gen.int (Range.constantFrom 0 (-1000) 1000)
      pure (key2, value)
    pure (key, H.fromList subMapList)
  (key, map2) <- forAll . Gen.element $ oldValues
  pure (key, map2, oldValues)

prop_hashMapDiff2_delete1 :: Property
prop_hashMapDiff2_delete1 = property $ do
  -- pick a key to delete
  (key, map2, oldValues) <- hashMapDiff2KeysAndMaps 
  let oldHashMap = H.fromList . toList $ oldValues
  map2Keys <- case nonEmpty . fmap fst . H.toList $ map2 of
    Nothing -> annotate "second level map is empty..." >> failure
    Just x -> pure x
  let
    newHashMap = H.delete key oldHashMap
    expectedResult = pure
      (key, H.HashMapKeyDelete <$> map2Keys)
  -- The type ensures that the hash function is used
  -- to extract info from the input hash map of hashes,
  -- So we can simplify the test by testing with the
  -- identity hash function.
  H.hashMapDiff2 id (toList oldValues) (H.toList newHashMap) === expectedResult

prop_hashMapDiff2_delete2 :: Property
prop_hashMapDiff2_delete2 = property $ do
  -- pick a key to delete
  (key, map2, oldValues) <- hashMapDiff2KeysAndMaps
  let oldHashMap = H.fromList . toList $ oldValues
  (key, map2) <- forAll . Gen.element $ oldValues
  map2Entries <- case nonEmpty . H.toList $ map2 of
    Nothing -> annotate "second level map is empty..." >> failure
    Just x -> pure x
  (key2, _) <- forAll . Gen.element $ map2Entries
  newHashMap <- flip H.traverseWithKey oldHashMap $ \k map2' ->
    pure $ case key == k of
      False -> map2'
      True -> H.delete key2 map2'
  let
    expectedResult = pure
      (key, pure $ H.HashMapKeyDelete key2)
  -- The type ensures that the hash function is used
  -- to extract info from the input hash map of hashes,
  -- So we can simplify the test by testing with the
  -- identity hash function.
  H.hashMapDiff2 id (toList oldValues) (H.toList newHashMap) === expectedResult

prop_hashMapDiff2_new2 :: Property
prop_hashMapDiff2_new2 = property $ do
  keys <- forAll $ Gen.nonEmpty
    (Range.constant 1 100)
    (Gen.int $ Range.constant 0 2000)
  distinctKeys <- case nonEmpty . S.toList . S.fromList . toList $ keys of
    Nothing -> annotate "Got empty list of distinct keys from non empty list of keys (impossible)" >> failure
    Just x -> pure x
  oldValues <- forM distinctKeys $ \key -> do
    keys2 <- forAll $ Gen.nonEmpty
      (Range.constant 1 100)
      (Gen.int $ Range.constant 0 2000)
    let distinctKeys2 = S.toList . S.fromList . toList $ keys2
    subMapList <-forM distinctKeys2 $ \key2 -> do
      value <- forAll $ Gen.int (Range.constantFrom 0 (-1000) 1000)
      pure (key2, value)
    pure (key, H.fromList subMapList)
  let oldHashMap = H.fromList . toList $ oldValues
  -- pick a key to update
  (key, map2) <- forAll . Gen.element $ oldValues
  -- pick a new key to add to the next hashmap
  let map2Keys = fmap fst . H.toList $ map2
  -- The other keys were choen from the range 0-2000 so this should be guaranteed
  -- to be new to the map...
  newKey <- forAll $ Gen.int (Range.constantFrom 2500 2000 3000)
  newValue <- forAll $ Gen.int (Range.constantFrom 0 (-1000) 1000)
  map2Entries <- case nonEmpty . H.toList $ map2 of
    Nothing -> annotate "second level map is empty..." >> failure
    Just x -> pure x
  newHashMap <- flip H.traverseWithKey oldHashMap $ \k map2' ->
    pure $ case key == k of
      False -> map2'
      True -> H.insert newKey newValue map2'
  let
    expectedResult = pure
      (key, pure $ H.HashMapKeyNew newKey newValue newValue)
  -- The type ensures that the hash function is used
  -- to extract info from the input hash map of hashes,
  -- So we can simplify the test by testing with the
  -- identity hash function.
  H.hashMapDiff2 id (toList oldValues) (H.toList newHashMap) === expectedResult

tests :: IO Bool
tests = checkParallel $$discover
