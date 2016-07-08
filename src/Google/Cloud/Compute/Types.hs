{-# LANGUAGE DeriveGeneric #-}

module Google.Cloud.Compute.Types where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | The 'ProjectId' is a string which the user can freely chose when creating
-- a new project in the Google cloud. It is globally unique.
newtype ProjectId = ProjectId { unProjectId :: Text }


data ExportLocation = ExportLocation
    { bucketName       :: Text
    , reportNamePrefix :: Text
    } deriving (Generic)

instance FromJSON ExportLocation

data Quota = Quota
    { metric :: Text
    , limit  :: Double
    , usage  :: Double
    } deriving Generic

instance FromJSON Quota


data Metadata = Metadata
    { kind        :: Text
    , fingerprint :: Text
    , items       :: [Item]
    } deriving Generic

instance FromJSON Metadata
instance ToJSON Metadata


data Item = Item
    { key   :: Text
    , value :: Text
    } deriving Generic

instance FromJSON Item
instance ToJSON Item
