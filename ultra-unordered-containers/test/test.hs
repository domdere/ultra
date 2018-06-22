{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Test.Ultra.Data.HashMap.Strict

import Lab.Core.Main

import Preamble

main :: IO ()
main = labMain
    [   Test.Ultra.Data.HashMap.Strict.tests
    ]
