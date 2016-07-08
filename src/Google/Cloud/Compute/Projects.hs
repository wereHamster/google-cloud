{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Google.Cloud.Compute.Projects where

import           Blaze.ByteString.Builder    (Builder)
import           Control.Monad               (void)
import           Data.Aeson                  (FromJSON, encode)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           Network.HTTP.Types          (encodePath)

import           Google.Cloud.Compute.Types
import           Google.Cloud.Internal.HTTP
import           Google.Cloud.Internal.Token
import           Google.Cloud.Internal.Types

data ProjectResource = ProjectResource
    { kind                   :: Text
    , creationTimestamp      :: Text
    , name                   :: Text
    , commonInstanceMetadata :: Metadata
    , quotas                 :: [Quota]
    , selfLink               :: Text
    , defaultServiceAccount  :: Text
    } deriving (Generic)

instance FromJSON ProjectResource

getProject :: ProjectId -> Cloud ProjectResource
getProject projectId = do
    authH <- authorizationHeader
    getJSON url [authH]
  where
    url = computeUrl <> encodePath ["projects", unProjectId projectId] []


setCommonInstanceMetadata :: ProjectId -> [Item] -> Cloud ()
setCommonInstanceMetadata projectId itms = do
    authH <- authorizationHeader
    pr <- getProject projectId
    void $ post url [authH] (body pr)
  where
    url =
        computeUrl <>
        encodePath
            ["projects", unProjectId projectId, "setCommonInstanceMetadata"]
            []
    body pr =
        encode
            (Metadata
             { fingerprint = (fingerprint (commonInstanceMetadata pr))
             , items = itms
             })

computeUrl :: Builder
computeUrl = "https://www.googleapis.com/compute/beta"
