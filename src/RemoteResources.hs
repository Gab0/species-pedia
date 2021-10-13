{-# LANGUAGE OverloadedStrings #-}

module RemoteResources where

import           Network.HTTP.Conduit
import           Network.HTTP.Types.URI
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import           Data.Aeson
import           Text.HTML.TagSoup
import           Types

-- Fetch information on a single species from GBIF.
fetchInformationGBIF :: String -> IO LUTF8.ByteString
fetchInformationGBIF query =
  simpleHttp $ base_url ++ query_string
  where
    base_url = "https://api.gbif.org/v1/species/search"
    query_string = UTF8.toString
                 $ renderQuery True
                 [ ("q", Just $ UTF8.fromString query)
                 --, ("nameType", Just "INFORMAL")
                 ]

-- Decode GBIF response JSON according to our custom types
-- and their FromJSON instances.
decodeInformationGBIF :: LUTF8.ByteString -> Either String Types.RemoteResult
decodeInformationGBIF = eitherDecode


fetchObservationMapGBIF :: IO UTF8.ByteString
fetchObservationMapGBIF = return ""

biolibHost :: String
biolibHost = "https://www.biolib.cz"

downloadImages :: String -> IO LUTF8.ByteString
downloadImages query =
  simpleHttp $ base_url ++ query_string
  where
    base_url = biolibHost ++ "/en/formsearch/"
    query_string = UTF8.toString
                 $ renderQuery True
                 [ ("action"    , Just "execute")
                 , ("searcharea", Just "2")
                 , ("string"    , Just $ UTF8.fromString
                                       $ map convertQuery query)
                 ]
    convertQuery ' ' = '+'
    convertQuery u   = u

-- Locate relevant image urls in the raw HTML text
-- of the search page @biolib.cz.
parseImageUrls :: LUTF8.ByteString -> [String]
parseImageUrls content = map (biolibHost ++)
                       $ concatMap locateImageTags tags
  where
    tags = parseTags
         $ LUTF8.toString content

    -- Locate <img> tags among found tags.
    locateImageTags :: Tag String -> [String]
    locateImageTags (TagOpen "img" attr) = concatMap locateImageSrcInAttributes attr
    locateImageTags _                    = []

    -- Locate "src" parameters in HTML tags.
    locateImageSrcInAttributes :: Attribute String -> [String]
    locateImageSrcInAttributes ("src", src) = [src]
    locateImageSrcInAttributes _            = []
