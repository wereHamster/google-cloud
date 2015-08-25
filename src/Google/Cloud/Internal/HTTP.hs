{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Google.Cloud.Internal.HTTP where


import Control.Monad.Reader
import Control.Monad.Except

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)

import Data.Aeson

import Network.HTTP.Types.Header
import Network.HTTP.Client

import Google.Cloud.Internal.Types



runRequest :: Request -> Cloud ByteString
runRequest req = do
    manager <- asks hManager
    cloudIO $ do
        res <- httpLbs req manager
        return $ toStrict $ responseBody res


post :: String -> RequestHeaders -> ByteString -> Cloud ByteString
post url headers body = do
    req <- cloudIO $ do
        req <- parseUrl url
        return $ req
            { method         = "POST"
            , requestHeaders = headers
            , requestBody    = RequestBodyBS body
            }

    runRequest req


get :: String -> RequestHeaders -> Cloud ByteString
get url headers = do
    req <- cloudIO $ do
        req <- parseUrl url
        return $ req
            { method         = "GET"
            , requestHeaders = headers
            }

    runRequest req


getJSON :: (FromJSON a) => String -> RequestHeaders -> Cloud a
getJSON url headers = do
    body <- get url headers
    case eitherDecodeStrict body of
        Left e -> throwError $ DecodeError e
        Right r -> return r
