module Main where

import qualified Test.Ultra.Control.Applicative
import qualified Test.Ultra.Data.Either
import qualified Test.Ultra.Data.Foldable
import qualified Test.Ultra.Data.Ord
import qualified Test.Ultra.Data.RealFrac

import Lab.Core.Main

import Preamble

main :: IO ()
main = labMain
    [   Test.Ultra.Control.Applicative.tests
    ,   Test.Ultra.Data.Either.tests
    ,   Test.Ultra.Data.Foldable.tests
    ,   Test.Ultra.Data.Ord.tests
    ,   Test.Ultra.Data.RealFrac.tests
    ]
