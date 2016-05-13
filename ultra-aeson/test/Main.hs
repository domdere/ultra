{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Test.Ultra.Data.Aeson

import Lab.Core.Main

import Preamble

main :: IO ()
main = labMain
    [   Test.Ultra.Data.Aeson.tests
    ]
