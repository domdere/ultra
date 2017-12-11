{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.System.Verified.IO
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- A lot of this package is concerned with pulling out all the
-- possible exceptions and returning them as values of specific
-- types.
--
-- Exceptions and EitherT are two opposite extremes of a spectrum,
-- Exceptions are extremely convenient but unsafe.
--
-- Returning all possible errors using EitherT can get extremely
-- inconvenient but extremely safe.
--
-- This is an attempt at trying to find a middle ground.
--
-- With the EitherT approach, you sometimes have to handle errors
-- that should no longer be really likely at all.
--
-- For instance if you create a file, and then want to open it,
-- its should probably be fine to not have to handle the "Does not
-- exist" error.  It is of course possible that the file doesnt
-- exist when you want to open it, but this event should truly be
-- exceptional, and should not be expected to happen by design.
--
-- I.e under this approach, value level errors are for cases which
-- have not been excluded by design.  In this model, the ideal is
-- that exceptions are for events that have already had some measure
-- in place to prevent their occurence, if they do occur, its a bug
-- (e.g an race condition has occurred).
--
-- While if a value level error occurs, an environmental prerequisite
-- has not been satisfied
--
-------------------------------------------------------------------
module Ultra.System.Verified.IO (
        module X
    -- * Types
    ,   CreateDirectoryStyle(..)
    ,   UnusedPath
    ,   VerifiedFilePath
    ,   VerifiedDirPath
    -- * Functions
    ,   unusedPath
    ,   verifiedFilePath
    ,   verbatimVerifiedDirPath
    ,   verifiedDirPath
    ,   createDirectory
    ,   createDirectoryIfMissing
    ,   doesNotExist
    ,   existsAsDir
    ,   existsAsFile
    ,   listDirectory
    ,   openNewBinaryFile
    ,   openVerifiedBinaryFile
    ,   removeVerifiedDirectoryRecursive
    ,   withNewFileForWriting
    ,   withVerifiedBinaryFile
    ,   withTempDir
    ) where

import Ultra.Control.Monad.Bracket (MonadBracket(..), bracket', strictBracketEitherT)
import Ultra.Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Ultra.Control.Monad.Trans.Either (EitherT, bimapEitherT, left, runEitherT)
import Ultra.System.IO
import Ultra.System.Verified.IO.Error as X
import Ultra.System.Verified.IO.Handlers

import qualified Ultra.Data.Text as T

import qualified System.Directory as D
import qualified System.IO as S
import qualified System.Posix.Process as P

import Preamble

-- |
-- This token provides a proof that at some point in the past
-- some measure was taken to ensure that this is a valid location,
-- and nothing exists there (neither a file nor a directory). This could be
-- an existence check or an explicit delete dir or file operation.
--
-- Says nothing more, obviously because "computer", the value of this proof
-- diminishes quite rapidly after that step or check is made.  So best to use it
-- after the value has been determined, and to keep its region quite small,
-- unless you want to design that possibility out at the system level.
--
newtype UnusedPath = UnusedPath T.Text

unusedPath :: UnusedPath -> T.Text
unusedPath (UnusedPath p) = p

-- |
-- This represents a piece of text where some action has
-- been taken at some point to test or ensure it exists somewhere
-- as a file,
-- because "computers", this of course does not ensure
-- that it exists for all time, but this approach ensures
-- a well-intentioned design where this can only happen by
-- accident, and something will only be deleted just before
-- this value goes out of scope.
--
-- Simply put, a value of `VerifiedFilePath` is only a proof
-- that _some_ measure was taken to ensure or verify that
-- the location is valid and a file exists there at some point in the past.
-- It says nothing more.  Due to the nature of things, the value of this proof
-- diminishes quite quickly after that initial check or step is made, so its best to act on it
-- ASAP.  But in a well designed system it may be "good enough" for long durations...
--
newtype VerifiedFilePath = VerifiedFilePath T.Text

-- |
-- This represents a piece of text where some action has
-- been taken at some point to test or ensure it exists somewhere
-- as a directory,
-- because "computers", this of course does not ensure
-- that it exists for all time, but this approach ensures
-- a well-intentioned design where this can only happen by
-- accident, and something will only be deleted just before
-- this value goes out of scope.
--
newtype VerifiedDirPath = VerifiedDirPath T.Text

data CreateDirectoryStyle =
        CreateDirectoryParents
    |   DoNotCreateDirectoryParents
        deriving (Show, Eq)

verifiedFilePath :: VerifiedFilePath -> T.Text
verifiedFilePath (VerifiedFilePath t) = t

verbatimVerifiedDirPath :: VerifiedDirPath -> T.Text
verbatimVerifiedDirPath (VerifiedDirPath t) = t

-- | Drops trailing slashes...
--
verifiedDirPath :: VerifiedDirPath -> T.Text
verifiedDirPath (VerifiedDirPath t) = T.dropWhileEnd (== '/') t

doesNotExist
    :: (MonadIO m)
    => T.Text
    -> (UnusedPath -> m a)
    -> EitherT NonExistenceFail m a
doesNotExist p f = let sp = T.unpack p in do
    isFile  <- liftIO $ D.doesFileExist sp
    isDir   <- liftIO $ D.doesDirectoryExist sp
    when isFile . left $ ExistsAsFile p
    when isDir . left $ ExistsAsDir p
    lift . f . UnusedPath $ p

existsAsFile
    :: (MonadIO m)
    => T.Text
    -> (VerifiedFilePath -> m a)
    -> EitherT FileVerificationFail m a
existsAsFile fn f = do
    isFile <- liftIO $ D.doesFileExist (T.unpack fn)
    isDir <- liftIO $ D.doesDirectoryExist (T.unpack fn)
    case (isFile, isDir) of
        (False, False)  -> left $ FileVerificationDoesNotExist fn
        (False, True)   -> left $ FileVerificationIsDirectory fn
        (True, _)       -> lift . f . VerifiedFilePath $ fn

existsAsDir
    :: (MonadIO m)
    => T.Text
    -> (VerifiedDirPath -> m a)
    -> EitherT DirVerificationFail m a
existsAsDir dn f = do
    isDir <- liftIO $ D.doesDirectoryExist (T.unpack dn)
    isFile <- liftIO $ D.doesFileExist (T.unpack dn)
    case (isDir, isFile) of
        (False, False)  -> left $ DirVerificationDoesNotExist dn
        (False, True)   -> left $ DirVerificationIsFile dn
        (True, _)       -> lift . f . VerifiedDirPath $ dn

openVerifiedBinaryFile
    :: (MonadCatch m, MonadThrow m, MonadIO m)
    => VerifiedFilePath
    -> S.IOMode
    -> EitherT VerifiedFileOpenError m S.Handle
openVerifiedBinaryFile vf mode =
    liftIO (S.openBinaryFile (T.unpack . verifiedFilePath $ vf) mode) `catch` verifiedFileOpenHandler (verifiedFilePath vf) mode

-- |
-- A new file can only be opened for writing...
--
openNewBinaryFile
    :: (MonadCatch m, MonadThrow m, MonadIO m)
    => UnusedPath
    -> EitherT NewFileOpenError m S.Handle
openNewBinaryFile p =
    liftIO (S.openBinaryFile (T.unpack . unusedPath $ p) S.WriteMode) `catch` newFileOpenHandler (unusedPath p)

withVerifiedBinaryFile
    :: (MonadBracket m, MonadCatch m, MonadIO m)
    => VerifiedFilePath
    -> S.IOMode
    -> (S.Handle -> m a)
    -> EitherT VerifiedFileOpenError m a
withVerifiedBinaryFile vf mode f = bracket' (openVerifiedBinaryFile vf mode) hClose (lift . f)

-- |
-- After the write you will get a token that can be used for
-- verified operations...
--
-- This operation wont bother checking for exceptions that are excluded
-- under the assumption that the file doesnt exist (e.g Already in use))
--
-- Its probably important to note that on some operating systems,
-- if no data is written to the file, it will get deleted and
-- the guarantee is broken.  Its recommended that you only use this
-- in cases where you are sure you will be writing actual data to the file,
-- and use the non-verified functions for other cases...
--
withNewFileForWriting
    :: (MonadBracket m, MonadCatch m, MonadIO m)
    => UnusedPath
    -> (S.Handle -> m a)
    -> EitherT NewFileOpenError m (a, VerifiedFilePath)
withNewFileForWriting fn f = do
    x <- bracket' (openNewBinaryFile fn) hClose (lift . f)
    return (x, VerifiedFilePath $ unusedPath fn)

createDirectory
    :: (MonadCatch m, MonadIO m)
    => T.Text
    -> EitherT NewDirError m VerifiedDirPath
createDirectory dirName = do
    (liftIO $ D.createDirectory (T.unpack dirName)) `catch` (newDirErrorHandler dirName)
    pure . VerifiedDirPath $ dirName

-- |
-- does not include @./@ and @../@
listDirectory
    :: (MonadIO m)
    => VerifiedDirPath
    -> m [T.Text] -- ^ Their existence is "verified" but you dont know if they are files or directories, once i have used this a bit ill see if there is a useful type to use here...
listDirectory (VerifiedDirPath p) = do
    ds <- liftIO $ D.listDirectory (T.unpack p)
    pure (T.pack <$> ds)

createDirectoryIfMissing
    :: (MonadCatch m, MonadIO m)
    => CreateDirectoryStyle
    -> T.Text
    -> EitherT NewDirIfMissingError m VerifiedDirPath
createDirectoryIfMissing s dn =
    let
        b :: Bool
        b = case s of
            CreateDirectoryParents      -> True
            DoNotCreateDirectoryParents -> False
    in do
        (liftIO $ D.createDirectoryIfMissing b (T.unpack dn)) `catch` (newDirIfMissingErrorHandler dn)
        pure . VerifiedDirPath $ dn

removeVerifiedDirectoryRecursive
    :: (MonadCatch m, MonadIO m)
    => VerifiedDirPath
    -> EitherT RemoveVerifiedDirFail m UnusedPath
removeVerifiedDirectoryRecursive v =
    let
        p :: T.Text
        p = verifiedDirPath v
    in do
        (liftIO $ D.removeDirectoryRecursive (T.unpack p)) `catch` (removeVerifiedDirHandler p)
        pure . UnusedPath $ p

withTempDir
    :: forall m e a. (MonadBracket m, MonadCatch m, MonadIO m)
    => VerifiedDirPath
    -> T.Text -- ^ The PID will be appended to this, if the resulting dir already exists, the PID will be incremented until a non-existing dir is found.
    -> (VerifiedDirPath -> EitherT e m a)
    -> EitherT (WithTempDirError e a) m a
withTempDir parent dirPrefix f =
    let
        p :: T.Text
        p = verifiedDirPath parent

        findDirName :: Int32 -> m UnusedPath
        findDirName pid =
          (runEitherT $ doesNotExist (T.concat [p, "/", dirPrefix, T.pack . show $ pid]) pure) >>=
            either (const $ findDirName (pid + 1)) pure
    in bimapEitherT WithTempDirError snd $ strictBracketEitherT
        (do
          pid <- liftIO P.getProcessID
          dirName <- lift . findDirName . fromIntegral $ pid
          createDirectory . unusedPath $ dirName
        )
        removeVerifiedDirectoryRecursive
        f
