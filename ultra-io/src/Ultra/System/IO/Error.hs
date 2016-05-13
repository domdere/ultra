{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.System.IO.Error
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.System.IO.Error (
    -- * Types
        DirectoryCreateError(..)
    ,   DirectoryRecursiveCreateError(..)
    ,   FileOpenError(..)
    ,   RemoveDirectoryRecursiveError(..)
    -- * Functions
    ,   renderDirectoryCreateError
    ,   renderDirectoryRecursiveCreateError
    ,   renderFileOpenError
    ,   renderRemoveDirectoryRecursiveError
    ) where

import qualified Ultra.Data.Text as T

import System.IO ( IOMode(..) )

import Preamble

data FileOpenError =
        FileAlreadyInUse T.Text
    |   FileDoesNotExist T.Text
    |   FilePermissionError IOMode T.Text
        deriving (Show, Eq)

renderFileOpenError :: FileOpenError -> T.Text
renderFileOpenError (FileAlreadyInUse fn)       = T.concat ["file already in use: '", fn, "'"]
renderFileOpenError (FileDoesNotExist fn)       = T.concat ["file does not exist: '", fn, "'"]
renderFileOpenError (FilePermissionError m fn)  = T.concat ["file permissions do not permit ", (T.pack . show) m, " operation on '", fn, "'"]

data DirectoryCreateError =
        DirectoryParentsDontExist T.Text
    |   DirectoryCreateOther DirectoryRecursiveCreateError
        deriving (Show, Eq)

data DirectoryRecursiveCreateError =
        FileAlreadyExistsThere T.Text
    |   DirectoryCreateInsufficientPermissions T.Text
        deriving (Show, Eq)

renderDirectoryRecursiveCreateError :: DirectoryRecursiveCreateError -> T.Text
renderDirectoryRecursiveCreateError (FileAlreadyExistsThere dn)                 = T.concat ["could not create directory, file already exists there: '", dn, "'"]
renderDirectoryRecursiveCreateError (DirectoryCreateInsufficientPermissions dn) = T.concat ["insufficient permisisons to create directory here: '", dn, "'"]

renderDirectoryCreateError :: DirectoryCreateError -> T.Text
renderDirectoryCreateError (DirectoryParentsDontExist dn)   = T.concat ["could not create directory, one of the parent directories is missing: '", dn, "'"]
renderDirectoryCreateError (DirectoryCreateOther err)       = renderDirectoryRecursiveCreateError err

data RemoveDirectoryRecursiveError = RemoveDirectoryRecursiveNotADirectory T.Text deriving (Show, Eq)

renderRemoveDirectoryRecursiveError :: RemoveDirectoryRecursiveError -> T.Text
renderRemoveDirectoryRecursiveError (RemoveDirectoryRecursiveNotADirectory dname) = T.concat ["Cannot remove directory, it is not a directory: '", dname, "'"]
