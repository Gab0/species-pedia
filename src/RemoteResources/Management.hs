{-# LANGUAGE OverloadedStrings          #-}

module RemoteResources.Management where

import           Data.Default
import qualified Data.Text as T
import           Data.Either
import           Data.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as LUTF8

import           RemoteResources.GBIF
import           RemoteResources.Images
import           RemoteResources.Wikipedia

import Storage
import Types

-- | Manage remote content retrieval and the cache routines.
-- Fetch, insert to DB or retrieve from DB depending on
-- DB data availability.
manageCachedRemoteContent :: T.Text -> IO (Either String Types.RemoteResult)
manageCachedRemoteContent query_string = do
  cached_response <- loadFromDatabase $ sanitizeSpeciesName query_string

  case cached_response of
    Just response -> do
      putStrLn "Retrieving from database."
      return $ Right response
    Nothing       -> do
      putStrLn "Retrieving from the internet..."
      information <- decodeInformationGBIF
                 <$> fetchInformationGBIF (T.unpack query_string)
      case information of
        Left err                  -> return $ Left err
        Right (Types.GBIFSearchResult species_information) -> do
          new_record <- constructFullRecord species_information
          insertInDatabase new_record
          return      $ Right new_record

-- | Given a list of database records, upgrade the ones that are skeletons,
-- update the database accordingly and return all records involved, in full state.
upgradeRecordsIfRequired :: [Types.RemoteResult] -> IO [Types.RemoteResult]
upgradeRecordsIfRequired rr = do
  upgraded_records <- mapM upgradeRecord skel_records
  insertInDatabaseBatch upgraded_records
  return $ full_records ++ upgraded_records
  where
    full_records = filter ((== False) . remoteResultSkeletonState) rr
    skel_records = filter remoteResultSkeletonState                rr


-- | Fetch data for multiple species from GBIF, as skeleton records.
fetchRandomSpeciesBatch :: Int -> IO [Types.RemoteResult]
fetchRandomSpeciesBatch num = do
  id_list     <- readIDList
  putStrLn     $ "ID list loaded with " ++ show (length id_list) ++ " ids."

  contentGBIF <- sequence
               $ replicate num
               $ fetchRandomSpecies id_list

  let fetched_records = map (eitherDecode . LUTF8.fromString)
                      $ catMaybes contentGBIF

  print fetched_records
  return $ map constructSkeletonRecord
         $ rights fetched_records

-- | Converts a skeleton record into a full record.
upgradeRecord :: Types.RemoteResult -> IO Types.RemoteResult
upgradeRecord record
  | remoteResultSkeletonState record = constructFullRecord [remoteResultInformation record]
  | otherwise                        = return record

-- | Construct a skeleton record from a GBIF result.
-- Skeleton records contain basically only the GBIF result.
constructSkeletonRecord :: Types.SpeciesInformation -> Types.RemoteResult
constructSkeletonRecord species_information =
  def
    { Types.remoteResultScientificName = sanitizeSpeciesName
                                       $ speciesInformationScientificName species_information
    , Types.remoteResultInformation    = species_information
    }

-- | Construct a full record based on a GBIF result,
-- by retrieving missing information from additional sources.
constructFullRecord :: [Types.SpeciesInformation] -> IO Types.RemoteResult
constructFullRecord species_information = do
  let species_name =  sanitizeSpeciesName
                   $  Types.speciesInformationScientificName
                   $  head species_information
  image_urls      <-  retrieveImages    $ T.unpack species_name
  wikipedia       <-  retrieveWikipedia $ T.unpack species_name
  return $ Types.RemoteResult
    { Types.remoteResultOriginalQuery  = species_name -- FIXME: Maybe this field should be deprecated.
    , Types.remoteResultScientificName = species_name
    , Types.remoteResultInformation    = combineSpeciesInformation species_information
    , Types.remoteResultImages         = image_urls
    , Types.remoteResultWikipedia      = wikipedia
    , Types.remoteResultSkeletonState  = False
    }

-- | Some species names comes with all sorts of
-- strange tokens attached from GBIF,
-- such as year of discovery and/or discovery author names.
sanitizeSpeciesName :: T.Text -> T.Text
sanitizeSpeciesName name = takeScientificName
                         $ T.strip
                         $ T.toLower
                         $ foldl process name tokens
  where
    process str token    = head $ T.splitOn (T.singleton token) str
    tokens               = ",(" :: [Char]
    takeScientificName t = T.unwords $ take 2 $ T.words t
