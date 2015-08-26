module Google.Cloud
    ( Handle
    , createHandle, mkHandle

    , Cloud
    , evalCloud
    ) where


import Control.Concurrent.STM

import Network.HTTP.Client     (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Google.Cloud.Internal.Types
import Google.Cloud.Internal.Token



-- | Create a new 'Handle' with sensible defaults. The defaults are such that
-- the 'Handle' works out of the box when the application is running on an
-- instance in the Google cloud.

createHandle :: IO Handle
createHandle = do
    manager <- newManager tlsManagerSettings
    mkHandle manager defaultMetadataToken



-- | Create a new 'Handle' with your own configuration options.

mkHandle :: Manager -> Cloud Token -> IO Handle
mkHandle manager fetchToken = do
    token <- newTVarIO Nothing
    return $ Handle manager token fetchToken
