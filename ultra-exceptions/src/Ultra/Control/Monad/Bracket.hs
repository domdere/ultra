{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
-------------------------------------------------------------------
-- |
-- Module       : Ultra.Control.Monad.Bracket
-- Copyright    : (C) 2016
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Ultra.Control.Monad.Bracket (
    -- * Type Classes
        MonadBracket(..)
    -- * Types
    ,   StrictBracketError(..)
    ,   WeakBracketError(..)
    ,   MaskedBracketT(..)
    ,   SVBracket
    ,   SVBracket'
    ,   VBracket
    ,   VBracket'
    -- * Functions
    ,   esvbracket
    ,   liftSVBracket
    ,   liftVBracket
    ,   renderStrictBracketError
    ,   runWeakBracketResult
    -- ** MonadBracket Functions
    ,   evbracket
    ,   vbracket
    ,   bracket'
    ,   ebracket
    ,   strictBracketEitherT
    ) where

import Ultra.Control.Monad.Catch (MonadCatch(..), MonadThrow(..), MonadMask(..), maskedBracket)
import Ultra.Control.Monad.Trans.Either (EitherT, pattern EitherT, runEitherT)
import qualified Ultra.Data.Text as T

import Control.Monad.Trans.Writer (WriterT(..), tell)

import Preamble hiding (head)
import Ultra.Preamble

-- |
-- This would allow for a MonadBracket instance of StateT.
--
-- The @Maybe b@ allows for the possibility of state being threaded through for
-- `StateT`, @m(c, b)@ allows for the possibility of value level errors being handled.
--
-- This formulation of bracket is best for lifting through the various kinds of monad transformers
-- rather than having to rely on lifting the concepts of `MonadCatch`, `MonadThrow` and `MonadMask` through.
--
-- StateT relies on lifting `MonadCatch` to work with State, so that the bracket on `MonadMask` can work with `StateT`.
--
-- `EitherT` fails altogether since it can't satisfy `MonadMask` and the conventional bracket drops the result of the release
-- operation.
--
type SVBracket' m a b c = m a -> (Maybe b -> a -> m c) -> (a -> m b) -> m (c, b)

type SVBracket m = forall a b c. SVBracket' m a b c

-- |
-- Bracket operation that returns the release result too,
-- this is required for translating to EitherT, which has value level errors
--
type VBracket' m a b c = m a -> (a -> m c) -> (a -> m b) -> m (c, b)

type VBracket m = forall a b c. VBracket' m a b c

-- |
-- This is an error case for when you cannot work around the case
-- where the inside of the bracket failed, but the resource could not be released.
-- The successful value for the use is provided so that it can optionally be persisted in some
-- error handler.
--
data StrictBracketError b e e' e'' =
        AcquireFailed e
    |   UseFailedAndResourceLeak e' e''
    |   UseSucceededButResourceLeak b e''
    |   UseFailed e'
        deriving (Show, Eq)

renderStrictBracketError
    :: (b -> T.Text)
    -> (e -> T.Text)
    -> (e' -> T.Text)
    -> (e'' -> T.Text)
    -> StrictBracketError b e e' e''
    -> T.Text
renderStrictBracketError renderUseResult renderAcquireError renderUseError renderReleaseError =
    \case
        AcquireFailed acquireError                          -> T.concat ["failed to acquire resource: ", renderAcquireError acquireError]
        UseFailedAndResourceLeak useError releaseError      -> T.concat ["encountered error: '", renderUseError useError, "' and could not free resource either: '", renderReleaseError releaseError, "'"]
        UseSucceededButResourceLeak useResult releaseError  -> T.concat ["Successfully got result ('", renderUseResult useResult, "') but encountered error releasing resource: '", renderReleaseError releaseError, "'"]
        UseFailed useError                                  -> renderUseError useError


-- |
-- This is an error case where you want the EitherT to be less strict,
-- you only want it to fail if the use failed.
-- see `WeakBracketResult` for the other possibilities.
--
data WeakBracketError e e' e'' =
        WeakCouldNotAcquire e
    |   WeakUseFailedAndResourceLeak e' e''
    |   WeakUseFailed e'
        deriving (Show, Eq)

data WeakBracketResult e' c b =
        AllOk c b
    |   ResourceLeak e' b
        deriving (Show, Eq)

-- |
-- This is the intended use pattern for `WeakBracketResult`,
-- you will be intending to do something with the resource leak info and
-- moving on, preferably something more than @const $ return ()@.
--
runWeakBracketResult
    :: (Monad m)
    => WeakBracketResult e' c b
    -> (e' -> m ())
    -> (Maybe c -> b -> m a)
    -> m a
runWeakBracketResult (AllOk x y) _ f            = f (pure x) y
runWeakBracketResult (ResourceLeak err y) e f   = e err >> f Nothing y

liftSVBracket
    :: (forall a. m a -> n a)
    -> (forall a. n a -> m a)
    -> SVBracket m
    -> SVBracket n
liftSVBracket to fro svb acquire release use = to $ svb (fro acquire) ((fro .) . release) (fro . use)

liftVBracket
    :: (forall a. m a -> n a)
    -> (forall a. n a -> m a)
    -> VBracket m
    -> VBracket n
liftVBracket to fro vb acquire release use = to $ vb (fro acquire) (fro . release) (fro . use)

-- |
-- This typeclass is for monads which can provide some "assurance" (but not a guarantee, cos "computers")
-- that given a value to acquire a resource, an action to release that resource and an action to use that resource,
-- use will run after acquire, followed by release, even in the event of any notion of an "error",
-- be it an exception or value level error...
--
-- In cases where the release and use fail, the release error
-- takes precedence,
--
-- theres no good reason for this, this just mimics the semantics
-- of the exception based bracket on MonadMask...
--
-- I have yet to find a good expression for these properties as
-- laws :(
--
class (Monad m) => MonadBracket m where
    svbracket :: SVBracket m

newtype MaskedBracketT m a = MaskedBracketT {
        runMaskedBracketT :: m a
    }   deriving (Functor, Applicative, Monad, MonadCatch, MonadIO, MonadThrow)

instance MonadBracket Identity where
    svbracket acquire release use = do
        r <- acquire
        x <- use r
        y <- release (pure x) r
        pure (y, x)

instance MonadBracket IO where
    svbracket = maskedBracket

instance (MonadMask m) => MonadBracket (MaskedBracketT m) where
    svbracket = liftSVBracket MaskedBracketT runMaskedBracketT maskedBracket

instance (MonadBracket m) => MonadBracket (ExceptT e m) where
    svbracket = esvbracket svbracket

-- This sucks, `VBracket` can be modified to allow this to
-- be derived from MonadBracket instead of MonadMask though,
-- the release function must be modified to be @Maybe b -> a -> m c@,
-- so that the state can be threaded through...
instance (MonadBracket m) => MonadBracket (StateT s m) where
    svbracket = stateTBracket svbracket

instance (MonadBracket m) => MonadBracket (ReaderT r m) where
    svbracket acquire release use = ReaderT $ \r ->
        let
            f = flip runReaderT r
        in svbracket (f acquire) (\my x -> f $ release my x) (f . use)

instance (Monoid w, MonadBracket m) => MonadBracket (WriterT w m) where
    svbracket acquire release use =
        let
            release' my (r, w1) = runWriterT $ do
                -- If the result is nothing, the "use" failed, and the writer output
                -- of the acquire has not been accumulated,
                -- If there is a result, dont have to use the acquire writer result, its
                -- already included in @w2@
                tell $ maybe w1 snd my
                release (fst <$> my) r

            use' (r, w1) = runWriterT $ tell w1 >> use r
        in WriterT $ do
            ((z, w3), (y, w2)) <- svbracket (runWriterT acquire) release' use'
            return ((z, y), w2 `mappend` w3)

-- Lifts a stateful value bracket into `EitherT e`, in cases where the release and use both
-- result in value errors, precedence is given to the use error, the release error is dropped...
-- This is a strong flaw, if not critical (TODO),  This could result in resources leaked,
-- like maybe an expensive cluster of machines. In these cases it would be good to know the release
-- failed so that information can be provided to something or someone to ensure it gets released some other
-- way...
--
esvbracket
    :: forall m e a b c. (Monad m)
    => SVBracket m
    -> SVBracket' (EitherT e m) a b c
esvbracket svbracket' acquire release use =
    let
        acquire' :: m (Either e a)
        acquire' = runEitherT acquire

        release' :: Maybe (Either e b) -> Either e a -> m (Either e c)
        release' meb =
            let
                mb :: Maybe b
                mb = meb >>= head
            in either (pure . Left) (runEitherT . release mb) -- if acquire' failed pass through

        use' :: Either e a -> m (Either e b) -- if acquire' failed skip the use'
        use' = either (pure . Left) (runEitherT . use)
    in EitherT $ do
        (releaseResult, result) <- svbracket' acquire' release' use'
        return $ (,) <$> releaseResult <*> result

stateTBracket
    :: forall m s a b c. (Functor m)
    => SVBracket m
    -> SVBracket' (StateT s m) a b c
stateTBracket svb acquire release use =
        let
            acquire' :: s -> m (a, s)
            acquire' = runStateT acquire

            release' :: Maybe (b, s) -> (a, s) -> m (c, s)
            release' mbs (resource, s) =
                let
                    -- if use ran successully use its state, otherwise use the state from the acquire (this is what happens in the `MonadCatch` instance for `StateT`)
                    st :: s
                    st = maybe s snd mbs

                    mb :: Maybe b
                    mb = fst <$> mbs
                in runStateT (release mb resource) st

            use' :: (a, s) -> m (b, s)
            use' (resource, st) = runStateT (use resource) st

            -- This is for the case when everything succeeds,
            -- release should have been run last, so take its state value...
            --
            selectFinalState :: ((c, s), (b, s)) -> ((c, b), s)
            selectFinalState ((rel, s'), (result, _)) = ((rel, result), s')

        in StateT $ \s -> selectFinalState <$> svb (acquire' s) release' use'


-- MonadBracket functions

vbracket :: (MonadBracket m) => VBracket m
vbracket acquire release use = svbracket acquire (const release) use

-- | Generalized abstracted pattern of safe resource acquisition and release
-- in the face of exceptions and EitherT errors. The first action \"acquires\" some value, which
-- is \"released\" by the second action at the end. The third action \"uses\"
-- the value and its result is the result of the 'ebracket'.
--
-- If an exception occurs during the use, the release still happens before the
-- exception is rethrown.
--
-- The only time an EitherT error is expected to get dropped is when the
-- acquire has succeeded and the use has created its own EitherT error,
-- release will still get run, its just that if it returns an error, that will
-- get dropped.
--
ebracket
    :: forall m e a b c. (MonadBracket m)
    => EitherT e m a        -- ^ @acquire@
    -> (a -> EitherT e m c) -- ^ @release@
    -> (a -> EitherT e m b) -- ^ @use@
    -> EitherT e m b
ebracket acquire release use = snd <$> esvbracket svbracket acquire (const release) use

evbracket
    :: forall m e a b c. (MonadBracket m)
    => EitherT e m a        -- ^ @acquire@
    -> (a -> EitherT e m c) -- ^ @release@
    -> (a -> EitherT e m b) -- ^ @use@
    -> EitherT e m (c, b)
evbracket acquire release use = esvbracket svbracket acquire (const release) use

bracket' :: (MonadBracket m) => m a -> (a -> m c) -> (a -> m b) -> m b
bracket' acquire release use = snd <$> vbracket acquire release use

strictBracketEitherT
    :: forall m e e' e'' a b c. (MonadBracket m)
    => EitherT e m a            -- ^ acquire
    -> (a -> EitherT e'' m c)   -- ^ release
    -> (a -> EitherT e' m b)    -- ^ use
    -> EitherT (StrictBracketError b e e' e'') m (c, b)
strictBracketEitherT acquire release use =
    let
        release' :: a -> EitherT e m (Either e'' c)
        release' = lift . runEitherT . release

        use' :: a -> EitherT e m (Either e' b)
        use' = lift . runEitherT . use

        resolveErrors :: Either e (Either e'' c, Either e' b) -> Either (StrictBracketError b e e' e'') (c, b)
        resolveErrors (Left err)                        = Left $ AcquireFailed err
        resolveErrors (Right (Left rel, Left inner))    = Left $ UseFailedAndResourceLeak inner rel
        resolveErrors (Right (Left rel, Right inner))   = Left $ UseSucceededButResourceLeak inner rel
        resolveErrors (Right (Right _, Left inner))     = Left $ UseFailed inner
        resolveErrors (Right (Right rel, Right inner))  = Right (rel, inner)
    in EitherT . fmap resolveErrors . runEitherT $ evbracket acquire release' use'


