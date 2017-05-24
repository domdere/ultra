{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.System.Verified.IO.Error
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.System.Verified.IO.Error (
    -- * Types
        NonExistenceFail(..)
    ,   VerifiedFileOpenError(..)
    ,   NewFileOpenError(..)
    ,   NewDirError(..)
    ,   NewDirIfMissingError(..)
    ,   FileVerificationFail(..)
    ,   DirVerificationFail(..)
    ,   RemoveVerifiedDirFail(..)
    ,   WithTempDirError(..)
    -- * Functions
    ,   renderNonExistenceFail
    ,   renderVerifiedFileOpenError
    ,   renderNewFileOpenError
    ,   renderNewDirError
    ,   renderNewDirIfMissingError
    ,   renderFileVerificationFail
    ,   renderDirVerificationFail
    ,   renderRemoveVerifiedDirFail
    ,   renderWithTempDirError
    ) where

import Ultra.Control.Monad.Bracket (StrictBracketError, renderStrictBracketError)
import qualified Ultra.Data.Text as T

import System.IO ( IOMode(..) )

import Preamble

data NonExistenceFail =
        ExistsAsFile T.Text
    |   ExistsAsDir T.Text
        deriving (Show, Eq)

renderNonExistenceFail :: NonExistenceFail -> T.Text
renderNonExistenceFail (ExistsAsFile fn)    = T.concat ["'", fn, "' already exists as a file"]
renderNonExistenceFail (ExistsAsDir dn)     = T.concat ["'", dn, "' already exists as a directory"]

data VerifiedFileOpenError =
        VerifiedFileAlreadyInUse T.Text
    |   VerifiedFilePermissionError IOMode T.Text
        deriving (Show, Eq)

renderVerifiedFileOpenError :: VerifiedFileOpenError -> T.Text
renderVerifiedFileOpenError (VerifiedFileAlreadyInUse fn)       = T.concat ["file already in use: '", fn, "'"]
renderVerifiedFileOpenError (VerifiedFilePermissionError m fn)  = T.concat ["file permissions do not permit ", (T.pack . show) m, " operation on '", fn, "'"]

data NewFileOpenError = NewFilePermissionError T.Text deriving (Show, Eq)

renderNewFileOpenError :: NewFileOpenError -> T.Text
renderNewFileOpenError (NewFilePermissionError fn) = T.concat ["file permissions do not permit creating and writing file: '", fn, "'"]

newtype NewDirIfMissingError =
    NewDirPermissionError T.Text
    deriving (Show, Eq)

data NewDirError =
        NewDirPlainError NewDirIfMissingError
    |   NewDirAlreadyExists T.Text
        deriving (Show, Eq)

renderNewDirIfMissingError :: NewDirIfMissingError -> T.Text
renderNewDirIfMissingError (NewDirPermissionError dn)    = T.concat ["file permissions do not permit creating directory: '", dn, "'"]

renderNewDirError :: NewDirError -> T.Text
renderNewDirError (NewDirPlainError e)      = renderNewDirIfMissingError e
renderNewDirError (NewDirAlreadyExists dn)  = T.concat ["could not create directory as it already exists: '", dn, "'"]

newtype RemoveVerifiedDirFail = RemoveVerifiedDirPermissionError T.Text deriving (Show, Eq)

renderRemoveVerifiedDirFail :: RemoveVerifiedDirFail -> T.Text
renderRemoveVerifiedDirFail (RemoveVerifiedDirPermissionError dn) = T.concat ["could not delete directory ('", dn, "') due to a permissions issue..."]

data FileVerificationFail =
        FileVerificationDoesNotExist T.Text
    |   FileVerificationIsDirectory T.Text
        deriving (Show, Eq)

renderFileVerificationFail :: FileVerificationFail -> T.Text
renderFileVerificationFail (FileVerificationDoesNotExist fn)    = T.concat ["nothing exists here: '", fn, "'"]
renderFileVerificationFail (FileVerificationIsDirectory fn)     = T.concat ["directory exists where file was expected: '", fn, "'"]

data DirVerificationFail =
        DirVerificationDoesNotExist T.Text
    |   DirVerificationIsFile T.Text
        deriving (Show, Eq)

renderDirVerificationFail :: DirVerificationFail -> T.Text
renderDirVerificationFail (DirVerificationDoesNotExist dn)  = T.concat ["nothing exists here: '", dn, "'"]
renderDirVerificationFail (DirVerificationIsFile dn)        = T.concat ["file exists where file was expected: '", dn, "'"]

newtype WithTempDirError e a =
    WithTempDirError (StrictBracketError a NewDirError e RemoveVerifiedDirFail)
    deriving (Show, Eq)

renderWithTempDirError :: (e -> T.Text) -> (a -> T.Text) -> WithTempDirError e a -> T.Text
renderWithTempDirError renderError renderX (WithTempDirError e) =
    renderStrictBracketError renderX renderNewDirError renderError renderRemoveVerifiedDirFail e
