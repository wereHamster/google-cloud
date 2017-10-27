[![Hackage](https://img.shields.io/hackage/v/google-cloud.svg)](https://hackage.haskell.org/package/google-cloud)  [![Build Status](https://travis-ci.org/wereHamster/google-cloud.png)](https://travis-ci.org/wereHamster/google-cloud)
[![plot-light](http://stackage.org/package/google-cloud/badge/lts)](http://stackage.org/lts/package/google-cloud)
[![plot-light](http://stackage.org/package/google-cloud/badge/nightly)](http://stackage.org/nightly/package/google-cloud)

## Haskell library to access Google Cloud APIs

The library is incomplete. I only implemented the functions which I need
in my own projects. But the basic infrastructure is there so that adding new
API endpoints is relatively easy.


#### Access Token

To interact with the Google Cloud API you need an access token. The easiest way
to obtain one is through the metadata service. That only works on instances
which run in the Google Cloud and have a service account attached to them.


#### Cloud monad

All actions which interact with the Google Cloud API run in the `Cloud`
monad. The monad wraps `IO`, `Except` and `Reader`. You get convenience
functions to send out HTTP requests and parse responses. The monad also
acts as a cache for some frequently used values (such as the access token).
All synchronous exceptions are caught and transformed into an `Error`.


#### Handle

To run a `Cloud` action you need a `Handle`. Because it is relatively expensive
to create a handle, you should create one for your whole application. You can
either use a convenience function which creates a new `Handle` with sensible
defaults (`createHandle`), or you can use one which gives you more options to
customize the handle (`mkHandle`).



#### Example

The following code uploads a file onto a Google Cloud Storage bucket.

In particular, it will upload the contents`/temp/file1.tar.gz` and create a file called `data_2017/file1.tar.gz` in the bucket `test-bucket-123` (assuming the user has write credentials for it).

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Google.Cloud         (newHandle, evalCloud)
import Google.Cloud.Storage (Bucket(..), Name(..), uploadFile)

main :: IO ()
main = do
    let bucket = Bucket "test-bucket-123"
        gdir   = GCSObjDir "data_2017"
	fname  = "/temp/file1.tar.gz" 

    h <- createHandle
    evalCloud h $ do
        uploadFile bucket gdir fname Nothing

```



### Contributors

Marco Zocca (@ocramz)