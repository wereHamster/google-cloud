{-# LANGUAGE OverloadedStrings #-}

module Google.Cloud.Storage (uploadFile, downloadFile, Bucket(..)) where

import Control.Monad
import Control.Monad.Trans (liftIO)

import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Monoid

import Network.Mime (defaultMimeLookup, MimeType)

import Google.Cloud.Internal.Types
import Google.Cloud.Internal.HTTP
import Google.Cloud.Internal.Token

import System.FilePath.Posix

newtype Bucket = Bucket { unBucket :: String } deriving (Eq, Show)



-- | Upload a file to a GCS 'Bucket'. 
uploadFile ::
     Bucket       -- ^ Bucket name on GCS (e.g. `data-store-2017`)
  -> String       -- ^ Directory name on GCS (e.g. `foo/bar`; NB: no trailing /)
  -> FilePath     -- ^ Absolute path of the file to be uploaded
  -> Maybe String -- ^ Optional filename for the uploaded file; if `Nothing`, it defaults to the name on the local host 
  -> Cloud ()
uploadFile bucket gcsDir fname newname = do
    let contentType = mkMimeType fname
    body <- liftIO $ BS.readFile fname
    authH <- authorizationHeader
    void $ post uri [("Content-Type", contentType), authH] body
  where
    gcsName = mkStorageName fname gcsDir newname
    uri = "https://www.googleapis.com/upload/storage/v1/b/" <> unBucket bucket <> "/o?uploadType=media&name=" <> gcsName


-- | Download a file from a GCS bucket, and return its contents as a ByteString
downloadFile ::
     Bucket    -- ^ Bucket name on GCS 
  -> String    -- ^ Filename on GCS (e.g. `foo/bar/baz/myfile.txt`)
  -> Cloud ByteString   -- ^ File contents as a raw bytestring
downloadFile bucket name = do
  authH <- authorizationHeader
  get uri [authH] where
    uri = "https://www.googleapis.com/storage/v1/b/" <> unBucket bucket <> "/o/" <> name



-- | Helpers

mkStorageName :: FilePath -> String -> Maybe String -> String
mkStorageName fname gcsDir fnm = maybe (append fname0 gcsDir) (`append` gcsDir) fnm 
  where
    fname0 = takeFileName fname
    append n d = d ++ "/" ++ n

mkMimeType :: FilePath -> MimeType
mkMimeType = defaultMimeLookup . T.pack
