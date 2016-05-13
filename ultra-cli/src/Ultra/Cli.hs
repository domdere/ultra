{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Cli
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Cli (
    -- * Functions
        renderErrorAndDie
    ,   renderErrorAndDieWithErrorCode
    ) where

import Ultra.Control.Monad.Trans.Either (EitherT, eitherT)
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.IO as T

import System.Exit ( ExitCode(..), exitWith, exitFailure )
import System.IO ( stderr )

import Preamble

renderErrorAndDie :: forall e a. (e -> T.Text) -> EitherT e IO a -> IO a
renderErrorAndDie f =
    let
        handleError :: e -> IO a
        handleError err = T.hPutStrLn stderr (f err) >> exitFailure
    in eitherT handleError pure

renderErrorAndDieWithErrorCode :: forall e a. (e -> (T.Text, Int)) -> EitherT e IO a -> IO a
renderErrorAndDieWithErrorCode f =
    let
        handleError :: e -> IO a
        handleError err = let (msg, exitCode) = f err in T.hPutStrLn stderr msg >> exitWith (ExitFailure exitCode)
    in eitherT handleError pure
