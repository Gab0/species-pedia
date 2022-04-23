{-# LANGUAGE OverloadedStrings #-}

module RemoteResources.Images where

import           Data.List
import           Data.Maybe
import           Text.HTML.TagSoup

import           Network.HTTP.Conduit
import           Network.HTTP.Types.URI

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8

import Types

biolibHost :: String
biolibHost = "https://www.biolib.cz"

-- | Top level function to retrieve species images.
retrieveImages :: String -> IO (RemoteContent [String])
retrieveImages species_name = do
  image_page <- downloadImages species_name

  result_page <-
      case checkDisambiguationPage image_page of
          True  -> skipDisambiguationPage image_page
          False -> return image_page

  return $
    case parseImageUrls result_page of
      [] -> NotAvailable
      r  -> Retrieved r

-- | Locate relevant image urls in the raw HTML text
-- of the search page @biolib.cz.
parseImageUrls :: String -> [String]
parseImageUrls = map (biolibHost ++)
               . filterImageUrls
               . concatMap locateImageTags
               . parseTags
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

-- | Sometimes a search will lead to a 'disambiguation' page,
-- check if we're there.
checkDisambiguationPage :: String -> Bool
checkDisambiguationPage content =
  "New search" `isInfixOf` content

-- | Locate the link that leads to the top result in a disambiguation page.
-- Then fetch it's contents.
skipDisambiguationPage :: String -> IO String
skipDisambiguationPage content =  LUTF8.toString
                              <$> simpleHttp (biolibHost ++ url)
  where
    url = head $ mapMaybe checkDisambiguationTag tags
    tags = parseTags content
    checkDisambiguationTag :: Tag String -> Maybe String
    checkDisambiguationTag (TagOpen "a" href) =
      case href of
        [("href", k)] -> if "searchrecords" `isInfixOf` k then Just k else Nothing
        _             -> Nothing
    checkDisambiguationTag _                  = Nothing


-- | Retrieve the webpage that contains image results.
downloadImages :: String -> IO String
downloadImages query =  LUTF8.toString
                    <$> simpleHttp (base_url ++ query_string)

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
