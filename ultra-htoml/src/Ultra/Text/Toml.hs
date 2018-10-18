{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Text.Toml
-- Copyright    : (C) 2018
-- License      : BSD-style (see the file /LICENSE.md
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Text.Toml (
  -- * Re-exports
    module X
  -- * Types
  , TomlParser(..)
  , TomlArray(..)
  , TomlError(..)
  , TomlTable(..)
  , TomlTableArray(..)
  , TomlNode(..)
  -- * Operators
  , (.:)
  , (.:?)
  -- * Functions
  , asTomlArray
  , asTomlBool
  , asTomlDate
  , asTomlFloat
  , asTomlInteger
  , asTomlTable
  , asTomlText
  , asTomlTableArray
  , getTomlTableEmptyIfAbsent
  , renderTomlError
  , tomlNodes
  ) where

import qualified Ultra.Data.Text as T

import qualified Data.HashMap.Strict as H
import Data.List (reverse)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as V

import Text.Toml as X
import Text.Toml.Types

import Preamble

data TomlError =
    CouldNotFindNode [T.Text] -- ^ The path that was searched, with the segments reversed...
  | InvalidNodeType [T.Text] T.Text Node -- ^ Path (segments reversed..) it was found at, expected type, the node that was found
  | AlternativeErrors (NonEmpty TomlError)
  | EmptyFail -- ^ For the Monoid instance required for Alternative instance of the parser...
    deriving (Show, Eq)

renderTomlError :: TomlError -> T.Text
renderTomlError (CouldNotFindNode path) = T.concat ["(Toml) expected to find node at <", renderPath path, "> but found nothing"]
renderTomlError (InvalidNodeType path typ _) = T.concat ["(Toml) expecting a node of type ", typ, " at <", renderPath path, ">"]
renderTomlError (AlternativeErrors errors) = T.bracketedList "(Toml) one of the following errors: [" "]" "|" . fmap renderTomlError . toList $ errors
renderTomlError EmptyFail = "(Toml) error"


renderPath :: [T.Text] -> T.Text
renderPath = T.intercalate "." . reverse

newtype TomlParser a = TomlParser {
    unTomlParser :: Either TomlError a
  } deriving (Show, Eq, Functor, Applicative, Monad)

data TomlTable = TomlTable {
    ttPath  :: [T.Text] -- ^ Tracks the route taken from the root table (segments in reverse...)
  , ttTable :: Table
  } deriving (Show, Eq)

data TomlTableArray = TomlTableArray {
    ttaPath :: [T.Text]
  , ttaArray :: V.Vector TomlTable
  } deriving (Show, Eq)

data TomlNode = TomlNode {
    tnPath :: [T.Text] -- ^ Tracks the route taken from the root table
  , tnNode :: Node
  } deriving (Show, Eq)

data TomlArray = TomlArray {
    taPath :: [T.Text]
  , taArray :: V.Vector TomlNode
  } deriving (Show, Eq)

(.:) :: TomlTable -> T.Text -> TomlParser TomlNode
(TomlTable path tbl) .: name =
  let
    newPath :: [T.Text]
    newPath = name : path
  in maybe
    (TomlParser . Left . CouldNotFindNode $ newPath)
    (pure . TomlNode newPath)
    (H.lookup name tbl)

(.:?) :: TomlTable -> T.Text -> TomlParser (Maybe TomlNode)
(TomlTable path tbl) .:? name =
  let
    newPath :: [T.Text]
    newPath = name : path
  in pure
    . fmap (TomlNode newPath)
    $ H.lookup name tbl

asTomlTable
  :: TomlNode
  -> TomlParser TomlTable
asTomlTable (TomlNode path node) =
  case node of
    VTable t -> pure $ TomlTable path t
    n -> TomlParser . Left $ InvalidNodeType path "Table" n

getTomlTableEmptyIfAbsent
  :: TomlTable
  -> T.Text
  -> TomlParser TomlTable
getTomlTableEmptyIfAbsent t@(TomlTable path _) name =
  let
    newPath :: [T.Text]
    newPath = name : path
  in t .:? name >>= \case
    Nothing -> pure $ TomlTable newPath emptyTable
    Just n -> asTomlTable n

asTomlText
  :: TomlNode
  -> TomlParser T.Text
asTomlText (TomlNode path node) =
  case node of
    VString t -> pure t
    n -> TomlParser . Left $ InvalidNodeType path "Text" n

asTomlTableArray
  :: TomlNode
  -> TomlParser TomlTableArray
asTomlTableArray (TomlNode path node) =
  case node of
    VTArray ta -> -- V.fromList
        pure
      . TomlTableArray path
      . V.fromList
      . fmap (\(i, t) -> TomlTable ((T.pack . show $ i) : path) t)
      . zip ([0..] :: [Int])
      . toList
      $ ta
    n -> TomlParser . Left $ InvalidNodeType path "Table Array" n

asTomlInteger
  :: TomlNode
  -> TomlParser Int64
asTomlInteger (TomlNode path node) =
  case node of
    VInteger x -> pure x
    n -> TomlParser . Left $ InvalidNodeType path "Integer" n

asTomlFloat
  :: TomlNode
  -> TomlParser Double
asTomlFloat (TomlNode path node) =
  case node of
    VFloat x -> pure x
    n -> TomlParser . Left $ InvalidNodeType path "Float" n

asTomlBool
  :: TomlNode
  -> TomlParser Bool
asTomlBool (TomlNode path node) =
  case node of
    VBoolean b -> pure b
    n -> TomlParser . Left $ InvalidNodeType path "Boolean" n

asTomlDate
  :: TomlNode
  -> TomlParser UTCTime
asTomlDate (TomlNode path node) =
  case node of
    VDatetime dt -> pure dt
    n -> TomlParser . Left $ InvalidNodeType path "DateTime" n

asTomlArray
  :: TomlNode
  -> TomlParser TomlArray
asTomlArray (TomlNode path node) =
  case node of
    VArray a -> pure
      . TomlArray path
      . V.fromList
      . fmap (\(i, n) -> TomlNode ((T.pack . show $ i) : path) n)
      . zip ([0..] :: [Int])
      . toList
      $ a
    n -> TomlParser . Left $ InvalidNodeType path "Array" n

tomlNodes :: TomlTable -> [(T.Text, TomlNode)]
tomlNodes (TomlTable path hmap) = fmap (\(k, v) -> (k, TomlNode (k : path) v))
 . H.toList
 $ hmap
