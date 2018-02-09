{-# LANGUAGE NoImplicitPrelude #-}
module Ultra.Network.HTTP.Client (
  -- * re-exports
    module X
  -- * Types
  , HttpClientError(..)
  , HttpClientResponse(..)
  , JSONBodyParseError(..)
  -- * Functions
  , addRequestHeader
  , httpClientResponse
  , optionalHttpClientResponse
  , catchHttpClientError
  , tryParsingBody
  , checkResponseStatus
  , jsonError
  ) where

import Ultra.Control.Monad.Catch (MonadCatch(..))
import Ultra.Control.Monad.Trans.Either (EitherT, hoistEither, left)
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T

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

jsonError :: String -> JSONBodyParseError
jsonError = JSONBodyParseError . T.pack

httpClientResponse :: Response a -> HttpClientResponse a
httpClientResponse resp = case responseStatus resp of
  Status statusCode' _ ->
    -- http-client SHOULD be handling redirects,
    -- hence im not routing them through to HttpOk,
    -- if we get a 3XX response, its because it broke the max redirects,
    -- in which case things are probably not OK
    if (statusCode' < 300 && statusCode' >= 200) then HttpOk resp else HttpNotOk resp

optionalHttpClientResponse :: Response a -> Maybe (HttpClientResponse a)
optionalHttpClientResponse resp = case responseStatus resp of
  -- http-client SHOULD be handling redirects,
  -- hence im not routing them through to HttpOk,
  -- if we get a 3XX response, its because it broke the max redirects,
  -- in which case things are probably not OK
  Status statusCode' _ | statusCode' == 404                       -> Nothing
  Status statusCode' _ | statusCode' < 300 && statusCode' >= 200  -> pure $ HttpOk resp
  _                    | otherwise                                -> pure $ HttpNotOk resp

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
