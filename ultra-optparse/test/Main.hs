module Main where

import Control.Monad

import System.Exit
import System.IO

main :: IO ()
main = hSetBuffering stdout LineBuffering >> sequence
    [
    ] >>= \rs -> unless (and rs) exitFailure
