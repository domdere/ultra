-------------------------------------------------------------------
-- |
-- Module       : Ultra.System.IO.Handlers
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.System.IO.Handlers (
    -- * Functions
        createDirRecursiveHandler
    ,   openFileHandler
    ,   removeDirRecursiveHandler
    ) where

import Ultra.System.IO.Error

import Ultra.Control.Monad.Catch ( MonadThrow(..) )
import Ultra.Control.Monad.Trans.Either (EitherT, left)
import qualified Ultra.Data.Text as T

import qualified System.IO as S
import qualified System.IO.Error as E

import GHC.IO.Exception ( IOErrorType(InappropriateType) )

import Preamble

openFileHandler :: (MonadThrow m) => T.Text -> S.IOMode -> E.IOError -> EitherT FileOpenError m a
openFileHandler fn mode e
    | E.isAlreadyInUseError e   = left $ FileAlreadyInUse fn
    | E.isDoesNotExistError e   = left $ FileDoesNotExist fn
    | E.isPermissionError e     = left $ FilePermissionError mode fn
    | otherwise                 = throwM e -- At this point this exception ought to truly be exceptional....

createDirRecursiveHandler :: (MonadThrow m) => T.Text -> E.IOError -> EitherT DirectoryRecursiveCreateError m a
createDirRecursiveHandler dname e
    | E.isAlreadyExistsError e  = left $ FileAlreadyExistsThere dname
    | E.isPermissionError e     = left $ DirectoryCreateInsufficientPermissions dname
    | otherwise                 = throwM e

removeDirRecursiveHandler :: (MonadThrow m) => T.Text -> E.IOError -> EitherT RemoveDirectoryRecursiveError m ()
removeDirRecursiveHandler dname e
    | E.ioeGetErrorType e == InappropriateType  = left $ RemoveDirectoryRecursiveNotADirectory dname
    | otherwise                                 = throwM e
