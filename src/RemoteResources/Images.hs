{-# LANGUAGE OverloadedStrings #-}

module RemoteResources.Images where

import           Data.List
import           Data.Maybe
import           Text.HTML.TagSoup

import           Network.HTTP.Conduit
import           Network.HTTP.Types.URI

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8

biolibHost :: String
biolibHost = "https://www.biolib.cz"

-- | Locate relevant image urls in the raw HTML text
-- of the search page @biolib.cz.
parseImageUrls :: LUTF8.ByteString -> [String]
parseImageUrls = map (biolibHost ++)
               . filterImageUrls
               . concatMap locateImageTags
               . parseTags
               . LUTF8.toString
  where
    -- Discard images that are not species.
    -- i.e. Website logos, etc.
    filterImageUrls :: [String] -> [String]
    filterImageUrls = catMaybes . map approveUrl
      where
        approveUrl url =
          case "THN" `isInfixOf` url of
            True  -> Just url
            False -> Nothing

    -- Locate <img> tags among found tags.
    locateImageTags :: Tag String -> [String]
    locateImageTags (TagOpen "img" attr) = concatMap locateImageSrcInAttributes attr
    locateImageTags _                    = []

    -- Locate "src" parameters in HTML tags.
    locateImageSrcInAttributes :: Attribute String -> [String]
    locateImageSrcInAttributes ("src", src) = [src]
    locateImageSrcInAttributes _            = []


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
