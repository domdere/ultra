{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ultra.Network.HTTP.Client (
  -- * re-exports
    module X
  -- * Types
  , HttpClientError(..)
  , HttpClientResponse(..)
  , JSONBodyParseError(..)
  -- * Functions
  , addRequestHeader
  , dropStatus
  , drop404
  , httpClientResponse
  , optionalHttpClientResponse
  , catchHttpClientError
  , tryParsingBody
  , checkResponseStatus
  , jsonError
  , renderHttpClientError
  ) where

import Ultra.Control.Monad.Catch (MonadCatch(..), throwM)
import Ultra.Control.Monad.Trans.Either (EitherT, hoistEither, left)
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T

import Control.Exception.Base (Exception(..))
import qualified Data.ByteString as BS
import Data.CaseInsensitive (mk)

import Network.HTTP.Client as X
import Network.HTTP.Types as X

import Preamble

-- |
-- The error type when using http-client
--
data HttpClientError b c =
    HttpError !HttpException
  | BodyParseError !b
  | NotOkHttpResponse !(Response c)
    deriving (Show)

newtype JSONBodyParseError = JSONBodyParseError {
    jsonParseErrorText :: T.Text
  } deriving (Show, Eq)

data HttpClientResponse a =
    HttpNotOk !(Response a)
  | HttpOk !(Response a)
    deriving (Show, Eq)

renderHttpClientError
  :: (Show c)
  => (b -> T.Text)
  -> HttpClientError b c
  -> T.Text
renderHttpClientError _ (HttpError e) = T.concat ["HTTP Error: ", T.pack . show $ e]
renderHttpClientError f (BodyParseError e) = T.concat ["Error parsing the body: ", f e]
renderHttpClientError _ (NotOkHttpResponse r) = T.concat ["Received Error Response: ", T.pack . show $ r]

jsonError :: String -> JSONBodyParseError
jsonError = JSONBodyParseError . T.pack

httpClientResponse :: Response a -> HttpClientResponse a
httpClientResponse resp = case responseStatus resp of
  Status statusCode' _ ->
    -- http-client SHOULD be handling redirects,
    -- hence im not routing them through to HttpOk,
    -- if we get a 3XX response, its because it broke the max redirects,
    -- in which case things are probably not OK
    if statusCode' < 300 && statusCode' >= 200 then HttpOk resp else HttpNotOk resp

optionalHttpClientResponse :: Response a -> Maybe (HttpClientResponse a)
optionalHttpClientResponse resp = case responseStatus resp of
  -- http-client SHOULD be handling redirects,
  -- hence im not routing them through to HttpOk,
  -- if we get a 3XX response, its because it broke the max redirects,
  -- in which case things are probably not OK
  Status statusCode' _ | statusCode' == 404                       -> Nothing
  Status statusCode' _ | statusCode' < 300 && statusCode' >= 200  -> pure $ HttpOk resp
  _                                                               -> pure $ HttpNotOk resp

-- | For use adapting "exception" style code
--
dropStatus
  :: forall m a e. (Exception e, Applicative m, Monad m, MonadCatch m)
  => Status
  -> (e -> Maybe Status)
  -> m a
  -> m (Maybe a)
dropStatus s getStatus mx =
    let
        handler :: e -> m (Maybe a)
        handler e
            | getStatus e == pure s = pure Nothing
            | otherwise             = throwM e
    in (Just <$> mx) `catch` handler

drop404
  :: forall m a e. (Exception e, Applicative m, Monad m, MonadCatch m)
  => (e -> Maybe Status)
  -> m a
  -> m (Maybe a)
drop404 = dropStatus status404

addRequestHeader :: T.Text -> BS.ByteString -> Request -> Request
addRequestHeader name value req = req {
    requestHeaders = (mk . T.encodeUtf8 $ name, value) : requestHeaders req
  }

catchHttpClientError :: (MonadCatch m) => m a -> EitherT (HttpClientError b c) m a
catchHttpClientError mx =
  lift mx `catch` (left . HttpError)

checkResponseStatus
  :: (MonadCatch m)
  => m (Response a)
  -> EitherT (HttpClientError b a) m (Response a)
checkResponseStatus mx = do
  r <- catchHttpClientError mx
  case httpClientResponse r of
    (HttpNotOk x) -> left $ NotOkHttpResponse x
    (HttpOk x) -> pure x

tryParsingBody
  :: (MonadCatch m)
  => m (Response a)
  -> (a -> Either c b)
  -> EitherT (HttpClientError c a) m (Response b)
tryParsingBody mx f =
  checkResponseStatus mx >>=
    hoistEither . first BodyParseError . traverse f
