module Main where

import qualified Test.Ultra.Data.Text

import Lab.Core.Main

import System.IO ( IO )

main :: IO ()
main = labMain
    [   Test.Ultra.Data.Text.tests
    ]
