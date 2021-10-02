{-# LANGUAGE OverloadedStrings #-}

module RemoteResources where

import           Network.HTTP.Conduit
import           Network.HTTP.Types.URI
import           Data.Maybe
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import           Data.ByteString
import           Data.Aeson

import          Types

-- Fetch information on a single species.
fetchInformationGBIF :: String -> IO LUTF8.ByteString
fetchInformationGBIF query =
  simpleHttp $ base_url ++ query_string
  where
    base_url = "https://api.gbif.org/v1/species"
    query_string = UTF8.toString
                 $ renderQuery True [("name", Just $ UTF8.fromString query)]

-- Decode GBIF response JSON according to our custom types
-- and their FromJSON instances.
decodeInformationGBIF :: LUTF8.ByteString -> Either String Types.RemoteResult
decodeInformationGBIF content = eitherDecode content
