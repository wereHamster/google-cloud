{-# LANGUAGE OverloadedStrings #-}

module Google.Cloud.Storage where


import Control.Monad

import Network.HTTP.Types (Header)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC8

import Google.Cloud.Internal.Types
import Google.Cloud.Internal.HTTP
import Google.Cloud.Internal.Token


newtype Bucket = Bucket { unBucket :: String }

newtype Name = Name { unName :: String }


-- | Upload a 'ByteString' to a 'Bucket'. This is the simplest function
-- to upload something into a 'Bucket'.
uploadMedia :: Bucket -> Name -> ByteString -> [Header] -> Cloud ()
uploadMedia bucket name body header = do
    authH <- authorizationHeader
    void $ post url (contentLength : authH : header) body
  where
    url =
        "https://www.googleapis.com/upload/storage/v1/b/" <> unBucket bucket <>
        "/o?uploadType=media&name=" <>
        unName name
    contentLength = ("Content-Length", BSC8.pack (show (BSL.length body)))


-- | Donwload a 'ByteString' from a 'Bucket'.
-- See https://cloud.google.com/storage/docs/json_api/v1/how-tos/performance for set `Header`
getMedia :: Bucket -> Name -> [Header] -> Cloud ByteString
getMedia bucket name header = do
    authH <- authorizationHeader
    get url (authH : header)
  where
    url = storageUrl <> "b/" <> unBucket bucket <> "/o/" <> unName name <> "?alt=media"


storageUrl :: String
storageUrl = "https://www.googleapis.com/storage/v1/"
