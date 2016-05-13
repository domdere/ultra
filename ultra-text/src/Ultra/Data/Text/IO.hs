{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Data.Text.IO
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Data.Text.IO (
        module X
    -- * Functions
    ,   hPutStrUtf8
    ,   hPutStrLnUtf8
    ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.IO as X
import System.IO ( Handle )

import Preamble

hPutStrUtf8 :: (MonadIO m) => Handle -> T.Text -> m ()
hPutStrUtf8 h = liftIO . BS.hPutStr h . T.encodeUtf8

hPutStrLnUtf8 :: (MonadIO m) => Handle -> T.Text -> m ()
hPutStrLnUtf8 h t = traverse_ (hPutStrUtf8 h) [t, "\n"]

