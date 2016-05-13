module Main where

import qualified Test.Ultra.Control.Monad.Trans.Either

import Lab.Core.Main

import Preamble

main :: IO ()
main = labMain
    [   Test.Ultra.Control.Monad.Trans.Either.tests
    ]
