{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Cli.Data
-- Copyright    : (C) 2016
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Cli.Data (
    -- * Types
        BinaryVerbosity(..)
    ,   Verbosity(..)
    ,   DryRun(..)
    -- * Functions
    ,   runDryRun
    ,   simpleVerboseMsg
    ) where

import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T

import qualified Data.ByteString as BS
import System.IO (stderr)

import Preamble

data BinaryVerbosity = BQuiet | BVerbose deriving (Show, Eq)

data Verbosity = Quiet | Verbose Int deriving (Show, Eq)

data DryRun = DryRun | RealDeal deriving (Show, Eq)

runDryRun :: (MonadIO m) => DryRun -> [T.Text] -> m a -> m (Maybe a)
runDryRun RealDeal _ mx = pure <$> mx
runDryRun DryRun ts _ =
    let
        t :: T.Text
        t = T.snoc (T.intercalate "\n" . fmap (mappend "DRY-RUN: ") $ ts) '\n'
    in Nothing <$ liftIO (BS.hPutStr stderr $ T.encodeUtf8 t)

simpleVerboseMsg :: (MonadIO m) => BinaryVerbosity -> T.Text -> m ()
simpleVerboseMsg BQuiet _   = pure ()
simpleVerboseMsg BVerbose m = liftIO . BS.hPutStr stderr . T.encodeUtf8 . flip T.snoc '\n' $ m
