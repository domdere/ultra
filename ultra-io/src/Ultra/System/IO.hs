-------------------------------------------------------------------
-- |
-- Module       : Ultra.System.IO
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.System.IO (
    -- * Types
        S.Handle
    ,   S.IOMode(..)
    -- * Functions
    ,   createRecursiveDirectoryIfMissing
    ,   hClose
    ,   openBinaryFile
    ,   removeDirectoryRecursive
    ,   withBinaryFile
    ) where

import Ultra.System.IO.Error
import Ultra.System.IO.Handlers

import Ultra.Control.Monad.Bracket (MonadBracket, bracket')
import Ultra.Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Ultra.Control.Monad.Trans.Either (EitherT)
import qualified Ultra.Data.Text as T

import qualified System.Directory as D
import qualified System.IO as S

import Preamble

openBinaryFile
    :: (MonadCatch m, MonadThrow m, MonadIO m)
    => T.Text
    -> S.IOMode
    -> EitherT FileOpenError m S.Handle
openBinaryFile fn mode =
    liftIO (S.openBinaryFile (T.unpack fn) mode) `catch` openFileHandler fn mode

withBinaryFile :: (MonadBracket m, MonadCatch m, MonadThrow m, MonadIO m)
    => T.Text
    -> S.IOMode
    -> (S.Handle -> m a)
    -> EitherT FileOpenError m a
withBinaryFile fn mode f = bracket' (openBinaryFile fn mode) hClose (lift . f)

hClose :: (MonadIO m) => S.Handle -> m ()
hClose = liftIO . S.hClose

createRecursiveDirectoryIfMissing
    :: (MonadCatch m, MonadThrow m, MonadIO m) => T.Text -> EitherT DirectoryRecursiveCreateError m ()
createRecursiveDirectoryIfMissing dname =
   liftIO (D.createDirectoryIfMissing True (T.unpack dname)) `catch` createDirRecursiveHandler dname

removeDirectoryRecursive
    :: (MonadCatch m, MonadThrow m, MonadIO m)
    => T.Text
    -> EitherT RemoveDirectoryRecursiveError m ()
removeDirectoryRecursive dname =
    liftIO (D.removeDirectoryRecursive (T.unpack dname)) `catch` removeDirRecursiveHandler dname
