{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.System.Verified.IO.Handlers
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.System.Verified.IO.Handlers (
    -- * Functions
        newFileOpenHandler
    ,   newDirErrorHandler
    ,   newDirIfMissingErrorHandler
    ,   removeVerifiedDirHandler
    ,   verifiedFileOpenHandler
    ) where

import Ultra.Control.Monad.Catch ( MonadThrow(..) )
import Ultra.Control.Monad.Trans.Either (EitherT, left)
import qualified Ultra.Data.Text as T
import Ultra.System.Verified.IO.Error

import qualified System.IO.Error as E
import qualified System.IO as S

import Preamble

newFileOpenHandler
    :: (MonadThrow m)
    => T.Text
    -> E.IOError
    -> EitherT NewFileOpenError m a
newFileOpenHandler fn e
    | E.isPermissionError e     = left $ NewFilePermissionError fn
    | otherwise                 = throwM e

newDirErrorHandler
    :: (MonadThrow m)
    => T.Text
    -> E.IOError
    -> EitherT NewDirError m a
newDirErrorHandler dn e
    | E.isPermissionError e     = left . NewDirPlainError . NewDirPermissionError $ dn
    | E.isAlreadyExistsError e  = left . NewDirAlreadyExists $ dn
    | otherwise                 = throwM e

newDirIfMissingErrorHandler
    :: (MonadThrow m)
    => T.Text
    -> E.IOError
    -> EitherT NewDirIfMissingError m a
newDirIfMissingErrorHandler dn e
    | E.isPermissionError e     = left . NewDirPermissionError $ dn
    | otherwise                 = throwM e

removeVerifiedDirHandler
    :: (MonadThrow m)
    => T.Text
    -> E.IOError
    -> EitherT RemoveVerifiedDirFail m a
removeVerifiedDirHandler dn e
    | E.isPermissionError e = left $ RemoveVerifiedDirPermissionError dn
    | otherwise             = throwM e

verifiedFileOpenHandler
    :: (MonadThrow m)
    => T.Text
    -> S.IOMode
    -> E.IOError
    -> EitherT VerifiedFileOpenError m a
verifiedFileOpenHandler fn mode e
    | E.isAlreadyInUseError e   = left $ VerifiedFileAlreadyInUse fn
    | E.isPermissionError e     = left $ VerifiedFilePermissionError mode fn
    | otherwise                 = throwM e
