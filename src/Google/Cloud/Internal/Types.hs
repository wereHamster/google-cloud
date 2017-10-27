{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Google.Cloud.Internal.Types where


import Control.Applicative
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad.Reader
import Control.Monad.Except

import Data.Time
import Data.Text (Text)
import qualified Data.Text as T

import System.Random

import Network.HTTP.Client (Manager)

import Prelude



data Handle = Handle
    { hManager :: !Manager
      -- ^ Shared HTTP manager.

    , hToken :: !(TVar (Maybe Token))
      -- ^ Cache for the access token. Use 'accessToken' when within the 'Cloud'
      -- monad to access the token. That function will automatically refresh it
      -- when it is about to expire.

    , hFetchToken :: !(Cloud Token)
      -- ^ The action which is used to fetch a fresh access token.
    }


data Token = Token
    { tokenExpiresAt :: !UTCTime
    , tokenValue     :: !Text
    } deriving (Show)


data Error
    = UnknownError !Text
    | IOError !String
    | DecodeError !String
    deriving (Show)



newtype Cloud a = Cloud
    { runCloud :: ReaderT Handle (ExceptT Error IO) a
    } deriving (Functor, Applicative, Monad, MonadIO,
        MonadError Error, MonadReader Handle)

instance Alternative Cloud where
    empty = throwError $ UnknownError "empty"
    a <|> b = catchError a (const b)


-- | Evaluate a 'Cloud' action and return either the 'Error' or the result.
evalCloud :: Handle -> Cloud a -> IO (Either Error a)
evalCloud h m = (runExceptT $ runReaderT (runCloud m) h) `catch`
    (\e -> transformException (UnknownError . T.pack . show) e >>= return . Left)



-- | Transform an synchronous exception into an 'Error'. Async exceptions
-- are left untouched and propagated into the 'IO' monad.
transformException :: (SomeException -> Error) -> SomeException -> IO Error
transformException f e = case fromException e of
    Just async -> throwIO (async :: AsyncException)
    Nothing    -> return $ f e



-- | Run an 'IO' action inside the 'Cloud' monad, catch all synchronous
-- exceptions and transform them into 'Error's.
cloudIO :: IO a -> Cloud a
cloudIO m = do
    res <- liftIO $ (Right <$> m) `catch`
        (\e -> transformException (IOError . show) e >>= return . Left)

    case res of
        Left e -> throwError e
        Right  r -> return r



-- | Retry a 'Cloud' action multiple times before failing for good.
--
-- TODO: Make the retry count configurable
retry :: Cloud a -> Cloud a
retry = go 0
  where
    -- Hardcoded for the time being. The last delay is ~32 seconds. Total about
    -- 60 seconds.
    maxRetries = 5

    -- Exponential backoff with a bit of jitter. As per Google recommendation.
    randomDelay i = cloudIO $ do
        jitter <- getStdRandom (randomR (0,500000))
        threadDelay $ jitter + 1000000 * (floor $ (2 :: Float) ** fromIntegral i)

    go :: Int -> Cloud a -> Cloud a
    go i m
      | i > maxRetries = throwError $ UnknownError "retry: Too many retries"
      | otherwise = m <|> (randomDelay i >> go (i + 1) m)
