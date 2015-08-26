{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Google.Cloud.Internal.Token where


import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Reader

import Data.Text (Text)
import Data.Text.Encoding
import Data.Monoid
import Data.Time

import Network.HTTP.Types.Header

import Google.Cloud.Internal.Types
import Google.Cloud.Compute.Metadata

import Prelude



-- | Fetch the access token for the default service account from the local
-- metadata server. This only works when the code is running in the Google
-- cloud and the instance has a services account attached to it.

defaultMetadataToken :: Cloud Token
defaultMetadataToken = serviceAccountToken "default"



-- | Store the token in the cache. If the cache already contains a token,
-- the better one of the two is actually stored (where *better* is defined
-- as the one which expires later). So it is safe to call this function
-- even if you are unsure if the token you have is better than the one
-- which is already in the cache.
--
-- Returns the better token.

cacheToken :: Token -> Cloud Token
cacheToken token = do
    tokenTVar <- asks hToken
    cloudIO $ atomically $ do
        currentToken <- readTVar tokenTVar
        let newToken = case currentToken of
              Nothing -> token
              Just x  -> if tokenExpiresAt x > tokenExpiresAt token then x else token

        writeTVar tokenTVar (Just newToken)
        return newToken


refreshToken :: Cloud Token
refreshToken = do
    fetchToken <- asks hFetchToken
    token <- fetchToken
    cacheToken token



-- | Return the value of the access token. The function guarantees that the
-- token is valid for at least 60 seconds. Though you should not be afraid
-- to call the function frequently, it caches the token inside the 'Handle' so
-- there is very little overhead.

accessToken :: Cloud Text
accessToken = do
    tokenTVar <- asks hToken
    mbToken <- cloudIO $ atomically $ readTVar tokenTVar
    tokenValue <$> case mbToken of
        Nothing -> refreshToken
        Just t -> do
            now <- cloudIO $ getCurrentTime
            if now > addUTCTime (-60) (tokenExpiresAt t)
                then refreshToken
                else return t



-- | Construct a 'Header' that contains the authorization details. Such a header
-- needs to be supplied to all requsts which require authorization.
--
-- Not all requests require it. In particular, requests to the metadata server
-- don't.
authorizationHeader :: Cloud Header
authorizationHeader = do
    token <- accessToken
    return ("Authorization", "Bearer " <> encodeUtf8 token)
