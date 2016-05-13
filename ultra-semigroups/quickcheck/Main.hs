module Main where

import qualified Test.Ultra.Data.List.NonEmpty

import Data.Foldable ( and )

import System.Exit
import System.IO

import Preamble

main :: IO ()
main = hSetBuffering stdout LineBuffering >> sequence
    [   Test.Ultra.Data.List.NonEmpty.tests
    ] >>= \rs -> unless (and rs) exitFailure
