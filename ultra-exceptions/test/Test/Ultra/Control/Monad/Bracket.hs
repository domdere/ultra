{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Ultra.Control.Monad.Bracket
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Ultra.Control.Monad.Bracket where

import Ultra.Control.Monad.Bracket
import Ultra.Control.Monad.Catch

import Ultra.Control.Monad.Trans.Either (EitherT, left, runEitherT)

import Control.Exception.Base ( ErrorCall(..) )

import Lab.Core.Property
import Lab.Core.QuickCheck
import Lab.Core.QuickCheck.TH
import Lab.Core.IO

import Preamble
import Prelude ( error )

-- helpers

prop_maskedBracket :: Int -> Int -> Int -> Property
prop_maskedBracket = testVbracket $ \a r u -> maskedBracket a (const r) u

prop_liftVBracket :: Int -> Int -> Int -> Property
prop_liftVBracket = testVbracket (liftVBracket id id vbracket)

testVbracket :: VBracket IO -> Int -> Int -> Int -> Property
testVbracket vbracket' openResult releaseResult useResult =
    let
        openSuccess :: IO Int
        openSuccess = pure openResult

        openFail :: IO Int
        openFail = error "openFail"

        releaseSuccess :: Int -> IO Int
        releaseSuccess _ = pure releaseResult

        releaseFail :: Int -> IO Int
        releaseFail _ = error "releaseFail"

        useSuccess :: Int -> IO Int
        useSuccess _ = pure useResult

        useFail :: Int -> IO Int
        useFail _ = error "useFail"

        checkFailz :: String -> IO a -> IO Property
        checkFailz err mx = ((failWith "Unexpected pass") <$ mx) `catch` (\(ErrorCall s) -> pure $ s === err)

    in conjoin . fmap ioProperty $ [
            (=== (releaseResult, useResult)) <$> vbracket' openSuccess releaseSuccess useSuccess
        ,   checkFailz "openFail" (vbracket' openFail releaseSuccess useSuccess)
        ,   checkFailz "useFail" (vbracket' openSuccess releaseSuccess useFail)
        ,   checkFailz "releaseFail" (vbracket' openSuccess releaseFail useSuccess)
        ,   checkFailz "releaseFail" (vbracket' openSuccess releaseFail useFail)
        ]

prop_ebracket :: Int -> Int -> Int -> Int -> Property
prop_ebracket openResult openError closeError innerError =
    let
        openSuccess :: EitherT Int IO Int
        openSuccess = pure openResult

        openFail :: EitherT Int IO Int
        openFail = left openError

        closeSuccess :: Int -> EitherT Int IO ()
        closeSuccess = const $ pure ()

        closeFail :: Int -> EitherT Int IO ()
        closeFail = const $ left closeError

        innerSuccess :: Int -> EitherT Int IO Property
        innerSuccess x = pure $ x === openResult

        innerFail :: Int -> EitherT Int IO Property
        innerFail = const $ left innerError

    in conjoin . fmap (ioProperty . (\(o, c, i, f) -> f <$> runEitherT (ebracket o c i)))$ [
            (openSuccess, closeSuccess, innerSuccess, either (const $ failWith "ebracket success failed...") id)
        ,   (openSuccess, closeSuccess, innerFail, either (=== innerError) (const $ failWith "ebracket failure was expected"))
        ,   (openSuccess, closeFail, innerSuccess, either (=== closeError)(const $ failWith "ebracket failure was expected"))
        ,   (openSuccess, closeFail, innerFail, either (=== closeError) (const $ failWith "ebracket failure was expected"))
        ,   (openFail, closeSuccess, innerSuccess, either (=== openError) (const $ failWith "ebracket failure was expected"))
        ,   (openFail, closeSuccess, innerFail, either (=== openError) (const $ failWith "ebracket failure was expected"))
        ,   (openFail, closeFail, innerSuccess, either (=== openError) (const $ failWith "ebracket failure was expected"))
        ,   (openFail, closeFail, innerFail, either (=== openError) (const $ failWith "ebracket failure was expected"))
        ]

return []
tests :: IO Bool
tests = $quickCheckAll
