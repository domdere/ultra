{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.System.Console.Haskeline.MonadException
-- Copyright    : (C) 2017 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.System.Console.Haskeline.MonadException (
  -- * Re-exports
    module X
  -- * Types
  , MEitherT(..)
  ) where

import Ultra.Control.Monad.Trans.Either (
    EitherT
  , pattern EitherT
  , runEitherT
  )

import System.Console.Haskeline.MonadException as X

import Preamble

-- |
-- <https://github.com/judah/haskeline/pull/22>
-- For reasons explained in the above link,
-- a MonadException instance for ExceptT
-- was not defined (the ticket mentions that
-- was added but wrapped in a CPP pragma but
-- at the time of this writing i couldnt find it)
--
-- Until theres a good resolution for it, I'll use this
--
newtype MEitherT e m a = MEitherT {
    unMEitherT :: EitherT e m a
  } deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadException m) => MonadException (MEitherT e m) where
  --controlIO :: (RunIO m -> IO (m a)) -> m a
    controlIO f =
      MEitherT . EitherT $ controlIO $ \(RunIO run) ->
        let
          run' = RunIO (fmap (MEitherT . EitherT) . run . runEitherT . unMEitherT)
        in runEitherT . unMEitherT <$> f run'
