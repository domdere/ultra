module Main where

import qualified Test.Ultra.Data.List.NonEmpty.Lens

import Lab.Core.Main

import System.IO ( IO )

import Preamble

main :: IO ()
main = labMain
    [   Test.Ultra.Data.List.NonEmpty.Lens.tests
    ]
