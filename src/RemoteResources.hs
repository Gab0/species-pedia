{-# LANGUAGE OverloadedStrings #-}

module RemoteResources where

import           Network.HTTP.Conduit
import           Network.HTTP.Types.URI
import           Data.List
import           Data.Maybe
import qualified Data.Text as T
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

-- | Locate relevant image urls in the raw HTML text
-- of the search page @biolib.cz.
parseImageUrls :: LUTF8.ByteString -> [String]
parseImageUrls content = map (biolibHost ++)
                       $ filterImageUrls
                       $ concatMap locateImageTags tags
  where
    tags = parseTags
         $ LUTF8.toString content

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


genusFields :: [(String, Types.SpeciesInformation -> Maybe T.Text)]
genusFields =
  [ ("Kingdom" , Types.speciesInformationKingdom)
  , ("Phylum"  , Types.speciesInformationPhylum)
  , ("Order"   , Types.speciesInformationOrder)
  , ("Genus"   , Types.speciesInformationGenus)
  , ("Family"  , Types.speciesInformationFamily)
  ]

-- | This functions evaluates a single
-- species result to see if it contains enough information
-- to be worth displaying on the website.
-- Criteria: Have all Taxonomic degree names
--        OR have any vernacular names attached
--        OR have any preservation status
evaluateInformation :: Types.SpeciesInformation -> Bool
evaluateInformation w = all_taxons
                     || any_vernacular
                     || any_status
  where
    all_taxons     = length present_fields == length genusFields
    any_vernacular = not $ null
                   $ Types.speciesInformationVernacularNames w
    any_status     = not $ null
                   $ Types.speciesInformationStatuses w
    present_fields = map (fetchClassification w) genusFields
    fetchClassification record (_, getter) = getter record

-- | Since GBIF multiple search results are generally redundant,
-- we have this function to merge all results into one.
combineSpeciesInformation :: [Types.SpeciesInformation] -> Types.SpeciesInformation
combineSpeciesInformation sp_info =
  foldl combine (head sp_info) (tail sp_info)
  where
    combine :: Types.SpeciesInformation -> Types.SpeciesInformation -> Types.SpeciesInformation
    combine a b = a <> b
