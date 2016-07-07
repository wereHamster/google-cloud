{-# LANGUAGE OverloadedStrings #-}

module Google.Cloud.Compute.Metadata where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except

import           Blaze.ByteString.Builder    (Builder)
import           Data.Aeson
import           Data.ByteString.Lazy        (ByteString, split, toStrict)
import           Data.ByteString.Lazy.Char8  (unpack)
import           Data.Char
import qualified Data.HashMap.Strict         as HMS
import           Data.Monoid
import           Data.Scientific
import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Time
import           Network.HTTP.Types          (encodePathSegments,
                                              encodePathSegmentsRelative)

import           Google.Cloud.Internal.HTTP
import           Google.Cloud.Internal.Types

import           Prelude



metadataServer :: Builder
metadataServer = "http://metadata.google.internal"

projectMetadataPath :: Builder
projectMetadataPath = "/computeMetadata/v1/project"

instanceMetadataPath :: Builder
instanceMetadataPath = "/computeMetadata/v1/instance"


-- | Convenience function to read a metadata value from the server. When
-- talking to the metadata server one has to supply a @Metadata-Flavor@ header,
-- otherwise the server refuses to communicate.
readKey :: Builder -> Cloud ByteString
readKey key =
    get
        (metadataServer <> key)
        [("Metadata-Flavor", "Google")]


-- | Like 'getJSON' but for reading from the metadata server.
readJSON :: (FromJSON a) => Builder -> Cloud a
readJSON key =
    getJSON
        (metadataServer <> key)
        [("Metadata-Flavor", "Google")]


-- | The 'ProjectId' is a string which the user can freely chose when creating
-- a new project in the Google cloud. It is globally unique.
newtype ProjectId = ProjectId { unProjectId :: Text }

projectId :: Cloud ProjectId
projectId =
    ProjectId . decodeUtf8 . toStrict <$>
    readKey (projectMetadataPath <> "/project-id")



-- | The 'NumericProjectId' can also be used to refer to a project on Google
-- cloud. It is globally unique.
newtype NumericProjectId = NumericProjectId { unNumericProjectId :: Integer }

numericProjectId :: Cloud NumericProjectId
numericProjectId =
    readKey (projectMetadataPath <> "/numeric-project-id") >>=
    (cloudIO . return . NumericProjectId . read . unpack)



-- | A project or instance metadata attribute is a key-value pair.
type Attribute = (ByteString, ByteString)

projectAttributes :: Cloud [Attribute]
projectAttributes = do
    let baseKey = projectMetadataPath <> "/attributes/"
    keys <- split (fromIntegral $ ord '\n') <$> readKey baseKey
    forM keys $
        \key ->
             ((,) key) <$>
             readKey
                 (baseKey <> encodePathSegmentsRelative [decodeUtf8 (toStrict key)])



-- | The ID of an instance. This is a unique, numerical ID that is generated
-- by Google Compute Engine. This is useful for identifying instances if you do
-- not want to use instance names.
newtype InstanceId = InstanceId { unInstanceId :: Integer }

instanceId :: Cloud InstanceId
instanceId =
    readKey (instanceMetadataPath <> "/id") >>=
    (cloudIO . return . InstanceId . read . unpack)



-- | The fully-qualified machine type name of the instance's host machine.
newtype MachineType = MachineType { unMachineType :: Text }

machineType :: Cloud MachineType
machineType =
    MachineType . decodeUtf8 . toStrict <$>
    readKey (instanceMetadataPath <> "/machine-type")



-- | The internal hostname of the instance.
internalHostname :: Cloud String
internalHostname = unpack <$> readKey (instanceMetadataPath <> "/hostname")


-- | The instance's zone.
newtype Zone = Zone { unZone :: Text }

zone :: Cloud Zone
zone =
    Zone . decodeUtf8 . toStrict <$> readKey (instanceMetadataPath <> "/zone")



-- | Fetch an access token for the given service account.
serviceAccountToken :: Text -> Cloud Token
serviceAccountToken acc = do
    res <-
        readJSON
            (instanceMetadataPath <>
             encodePathSegments ["service-accounts", acc, "token"])
    case res of
        (Object o) ->
            case (HMS.lookup "access_token" o, HMS.lookup "expires_in" o) of
                (Just (String value),Just (Number expiresIn)) -> do
                    case toBoundedInteger expiresIn :: Maybe Int of
                        Nothing ->
                            throwError $
                            UnknownError "fetchToken: Bad expiration time"
                        Just i -> do
                            now <- cloudIO getCurrentTime
                            return $
                                Token (addUTCTime (fromIntegral i) now) value
                _ ->
                    throwError $
                    UnknownError "fetchToken: Could not decode response"
        _ -> throwError $ UnknownError "fetchToken: Bad resposnse"
