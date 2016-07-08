{-# LANGUAGE OverloadedStrings #-}
module Google.Cloud.Internal.HTTP where


import           Control.Monad.Except
import           Control.Monad.Reader


import           Data.Aeson

import           Network.HTTP.Client
import           Network.HTTP.Types.Header

import           Google.Cloud.Internal.Types

import           Blaze.ByteString.Builder    (Builder, toByteString)
import           Data.ByteString.Lazy        (ByteString)

import qualified Data.ByteString.Char8       as BSC8



runRequest :: Request -> Cloud ByteString
runRequest req = do
    manager <- asks hManager
    cloudIO $ do
        res <- httpLbs req manager
        return $ responseBody res


post :: Builder -> RequestHeaders -> ByteString -> Cloud ByteString
post url headers body = do
    req <- cloudIO $ do
        req <- parseUrl (builderToString url)
        return $ req
            { method         = "POST"
            , requestHeaders = (requestHeaders req ++ headers)
            , requestBody    = RequestBodyLBS body
            }

    runRequest req


get :: Builder -> RequestHeaders -> Cloud ByteString
get url headers = do
    req <- cloudIO $ do
        req <- parseUrl (builderToString url)
        return $ req
            { method         = "GET"
            , requestHeaders = (requestHeaders req ++ headers)
            }

    runRequest req


getJSON :: (FromJSON a) => Builder -> RequestHeaders -> Cloud a
getJSON url headers = do
    body <- get url headers
    case eitherDecode body of
        Left e -> throwError $ DecodeError e
        Right r -> return r


builderToString :: Builder -> String
builderToString = BSC8.unpack . toByteString
