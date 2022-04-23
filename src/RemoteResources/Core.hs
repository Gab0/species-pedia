
module RemoteResources.Core where

import           Network.HTTP.Types.Status
import           Network.HTTP.Conduit

import qualified Data.ByteString.Lazy.UTF8 as LUTF8

-- | Execute a get request and allow it to fail,
-- probably due to 404 errors.
makeGetRequest :: String -> IO (Maybe String)
makeGetRequest url = do
  request  <- parseRequest url
  manager  <- newManager tlsManagerSettings
  response <- httpLbs request manager

  return $ case responseStatus response of
    (Status 200 _) -> Just $ LUTF8.toString
                    $ responseBody response
    _              -> Nothing
