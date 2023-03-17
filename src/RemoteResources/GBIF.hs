{-# LANGUAGE OverloadedStrings #-}

module RemoteResources.GBIF where

import           System.IO

import           Network.HTTP.Conduit

import           Network.HTTP.Types.URI

import           Data.Default
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8

import           Data.Aeson
import           RemoteResources.Core
import           Storage
import           Types


-- | Base URL for the species resource in the GBIF's API.
baseUrlGBIF :: String
baseUrlGBIF ="https://api.gbif.org/v1/species/"

-- | Fetch information on a single species from GBIF.
fetchInformationGBIF :: String -> IO LUTF8.ByteString
fetchInformationGBIF query =
  simpleHttp $ base_url ++ query_string
  where
    base_url = baseUrlGBIF ++ "search"
    query_string = UTF8.toString
                 $ renderQuery True
                 [ ("q", Just $ UTF8.fromString query)
                 ]

-- | Select a random ID from the ID list and try to fetch it from GBIF.
fetchRandomSpecies :: [String] -> IO (Maybe String)
fetchRandomSpecies all_id_list = do
  species_id    <- choice all_id_list
  case species_id of
    Just sid -> makeGetRequest $ baseUrlGBIF ++ sid
    Nothing  -> return Nothing


-- | Decode GBIF response JSON according to our custom types
-- and their FromJSON instances.
decodeInformationGBIF :: LUTF8.ByteString -> Either String GBIFSearchResult
decodeInformationGBIF = eitherDecode

fetchObservationMapGBIF :: IO UTF8.ByteString
fetchObservationMapGBIF = return ""

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
combineSpeciesInformation []      = def
combineSpeciesInformation sp_info =
  foldl combine (head sp_info) (tail sp_info)
  where
    combine :: Types.SpeciesInformation -> Types.SpeciesInformation -> Types.SpeciesInformation
    combine a b = a <> b

-- | Read the ID list from a file containing GBIF species IDs.
readIDList :: IO [String]
readIDList =  lines
          <$> (openFile "gbif_ids.txt" ReadMode
          >>= hGetContents)
