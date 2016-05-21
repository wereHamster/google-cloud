{-# LANGUAGE OverloadedStrings #-}

module Google.Cloud.Storage where


import Control.Monad

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson (FromJSON)
import Network.HTTP.Types (Header, encodePath)
import Data.ByteString.Lazy (ByteString)
import Blaze.ByteString.Builder (Builder)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC8

import Google.Cloud.Internal.Types
import Google.Cloud.Internal.HTTP
import Google.Cloud.Internal.Token

-- | See about fields
-- "https://cloud.google.com/storage/docs/json_api/v1/how-tos/performance#partial-response"
type Fields = BS.ByteString

newtype Bucket = Bucket { unBucket :: Text }

newtype Name = Name { unName :: Text }


-- | Upload a 'ByteString' to a 'Bucket'. This is the simplest function
-- to upload something into a 'Bucket'.
uploadMedia :: Bucket -> Name -> ByteString -> [Header] -> Cloud ()
uploadMedia bucket name body header = do
    authH <- authorizationHeader
    void $ post url (contentLength : authH : header) body
  where
    url =
        "https://www.googleapis.com/upload/storage/v1/b" <>
        encodePath
            [unBucket bucket, "o"]
            [ ("uploadType", Just "media")
            , ("name", Just (encodeUtf8 (unName name)))]
    contentLength = ("Content-Length", BSC8.pack (show (BSL.length body)))


-- | Donwload a 'ByteString' from a 'Bucket'.
-- See https://cloud.google.com/storage/docs/json_api/v1/how-tos/performance for set `Header`
getMedia :: Bucket -> Name -> [Header] -> Cloud ByteString
getMedia bucket name header = do
    authH <- authorizationHeader
    get url (authH : header)
  where
    url =
        storageUrl <>
        encodePath
            ["b", unBucket bucket, "o", unName name]
            [("alt", Just "media")]


-- | Retrieves a list of objects matching the criteria
-- (see "https://cloud.google.com/storage/docs/json_api/v1/objects/list")
list
    :: FromJSON a
    => Bucket -> Maybe BS.ByteString -> Maybe Fields -> Cloud a
list bucket prefix fields = do
    authH <- authorizationHeader
    getJSON url [authH]
  where
    url =
        storageUrl <>
        encodePath
             ["b", unBucket bucket, "o"]
             [("prefix", prefix), ("fields", fields)]

storageUrl :: Builder
storageUrl = "https://www.googleapis.com/storage/v1"
