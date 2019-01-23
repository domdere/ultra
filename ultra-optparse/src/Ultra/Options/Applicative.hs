{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Options.Applicative
-- Copyright    : (C) 2015 - 2018
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Options.Applicative (
  -- * Re-exports
    module X
  -- * Functions
  , command'
  , parseAndRun
  , eitherTextReader
  , enumWithDefaultP
  , envvar
  , envvarWithDefault
  , envvarWithDefaultWithRender
  , shimTextParser

  -- * Safe Command
  , SafeCommand (..)
  , runSafeCommand
  , safeCommand

  -- * Parser flags
  , bverbosity
  , dependency
  , dryrun
  , verbosity
  , version
  ) where

import Ultra.Cli.Data (BinaryVerbosity (..), DryRun (..), Verbosity (..))
import qualified Ultra.Data.Text as T

import qualified Data.List as L
import Options.Applicative as X

import System.Environment (getArgs)

import System.IO (print, putStrLn)

import Preamble

command' :: T.Text -> T.Text -> Parser a -> Mod CommandFields a
command' name description parser = command (T.unpack name) (info (parser <**> helper) (progDesc . T.unpack $ description))

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
      [] -> customExecParser (prefs showHelpOnError) (info (p <**> helper) topMods) >>= f
      _  -> execParser (info (p <**> helper) topMods) >>= f


enumWithDefaultP :: forall a. (Eq a) => a -> NonEmpty (a, T.Text, T.Text) -> Parser a
enumWithDefaultP def =
  let
    p :: a -> T.Text -> T.Text -> Parser a
    p fp long' help' = flag' fp $
          long (T.unpack long')
      <>  help (T.unpack help')
    f :: (a, T.Text, T.Text) -> Parser a -> Parser a
    f (x, long', help') parser =
      p x long' (help' <> (if x == def then " (Default)" else "")) <|> parser
  in foldr f (pure def)

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
    , " (can be set via environment variable "
    , env
    , ")"
    ]

-- | Safe Command with a dry-run mode, a dependencies and a version flag
data SafeCommand a =
    VersionCommand
  | DependencyCommand
  | RunCommand DryRun a
  deriving (Eq, Show)

-- | Run Safe Command
runSafeCommand :: Show t => SafeCommand t -> String -> String -> (t -> IO ()) -> IO ()
runSafeCommand sc v d b = case sc of
  VersionCommand ->
    putStrLn v
  DependencyCommand ->
    putStrLn d
  RunCommand DryRun c ->
    print c
  RunCommand RealDeal c ->
    b c

-- | Turn a Parser for a command of type a into a safe command
--   with a dry-run mode and a version flag
safeCommand :: Parser a -> Parser (SafeCommand a)
safeCommand commandParser =
      VersionCommand <$ version
  <|> DependencyCommand <$ dependency
  <|> RunCommand <$> dryrun <*> commandParser


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

-- | Flag for dry-run
dryrun :: Parser DryRun
dryrun = flag RealDeal DryRun $
      long "dry-run"
  <>  help "if set, suppresses any I/O that would make any persistent changes"

-- | Flag for listing dependencies.
dependency :: Parser ()
dependency = flag' () $
       long "dependencies"
    <> hidden

version :: Parser ()
version = flag' () $
       short 'v'
    <> long "version"
    <> help "version info."
