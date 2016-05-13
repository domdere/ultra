module Main where

import qualified Test.Ultra.Control.Lens
import qualified Test.Ultra.Data.List.Lens

import Lab.Core.Main

import Preamble

main :: IO ()
main = labMain
    [   Test.Ultra.Control.Lens.tests
    ,   Test.Ultra.Data.List.Lens.tests
    ]
