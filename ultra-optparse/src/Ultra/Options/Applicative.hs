{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Options.Applicative
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Options.Applicative (
        module X
    -- * Functions
    ,   command'
    ,   commandParser
    ,   parseAndRun
    ,   dryrun
    ,   eitherTextReader
    ,   envvar
    ,   envvarWithDefault
    ,   envvarWithDefaultWithRender
    ,   shimTextParser
    ,   bverbosity
    ,   verbosity
    ) where

import Ultra.Cli.Data (BinaryVerbosity(..), Verbosity(..), DryRun(..))
import qualified Ultra.Data.Text as T

import qualified Data.List as L
import Options.Applicative as X
import Options.Applicative.Builder.Internal (HasValue)

import System.Environment (getArgs)

import Preamble hiding ((<>))

command' :: T.Text -> T.Text -> Parser a -> Mod CommandFields a
command' name description parser = command (T.unpack name) (info (parser <**> helper) (progDesc . T.unpack $ description))

commandParser :: a -> [Mod CommandFields a] -> Parser a
commandParser versionCommand cs =
        flag' versionCommand (short 'v' <> long "version" <> help "version info.")
    <|> (subparser $ foldr (<>) mempty cs)

parseAndRun
    :: T.Text
    -> T.Text
    -> Parser a
    -> (a -> IO b)
    -> IO b
parseAndRun h desc p f =
    let
        topMods :: InfoMod a
        topMods = fullDesc
            <>  progDesc (T.unpack desc)
            <>  header (T.unpack h)
    in do
        x <- getArgs
        case x of
            -- If there were no commands, and the flags (the only valids ones in this case should be --version/-v and --help)
            -- are not recognised, then show the help msg.
            []  -> customExecParser (prefs showHelpOnError) (info (p <**> helper) topMods) >>= f
            _   -> execParser (info (p <**> helper) topMods) >>= f


shimTextParser :: (T.Text -> Either T.Text a) -> String -> Either String a
shimTextParser f = first T.unpack . f . T.pack

eitherTextReader :: (T.Text -> Either T.Text a) -> X.ReadM a
eitherTextReader = X.eitherReader . shimTextParser

-- |
-- There isnt really a better option...
-- This sucks in that if the env var is set but
-- cannot be parsed, it doesnt really give a nice error...
--
envvar
    :: (HasValue f)
    => (T.Text -> Maybe a)
    -> [(T.Text, T.Text)]
    -> T.Text
    -> T.Text
    -> X.Mod f a
envvar = envvar' mempty

envvarWithDefault
    :: (HasValue f, Show a)
    => (T.Text -> Maybe a)
    -> [(T.Text, T.Text)]
    -> T.Text
    -> a
    -> T.Text
    -> X.Mod f a
envvarWithDefault f envs env def h = envvar' (value def <> showDefault) f envs env h

envvarWithDefaultWithRender
    :: (HasValue f)
    => (T.Text -> Maybe a)
    -> (a -> T.Text)
    -> [(T.Text, T.Text)]
    -> T.Text
    -> a
    -> T.Text
    -> X.Mod f a
envvarWithDefaultWithRender f render envs env def h = envvar' (value def <> showDefaultWith (T.unpack . render)) f envs env h

envvar'
    :: (HasValue f)
    => X.Mod f a
    -> (T.Text -> Maybe a)
    -> [(T.Text, T.Text)]
    -> T.Text
    -> T.Text
    -> X.Mod f a
envvar' ifEmpty f envs env h =
    let
        o = maybe ifEmpty value (L.lookup env envs >>= f)
    in mappend o . help . T.unpack . T.concat $ [
            h
        ,   " (can be set via environment variable "
        ,   env
        ,   ")"
        ]

bverbosity :: Parser BinaryVerbosity
bverbosity = flag BQuiet BVerbose $
        long "verbose"
    <>  help "run verbosely"

verbosity :: Parser Verbosity
verbosity = option (Verbose <$> auto) $
        long "verbosity"
    <>  value Quiet
    <>  metavar "INT"
    <>  help "run verbosely"

dryrun :: Parser DryRun
dryrun = flag RealDeal DryRun $
        long "dry-run"
    <>  help "if set, suppresses any I/O that would make any persistent changes"
