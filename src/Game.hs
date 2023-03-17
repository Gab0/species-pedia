{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Game where

import           Yesod
import           Yesod.Form

import           Control.Monad

import           Data.List
import           Data.Maybe ( catMaybes, fromMaybe )
import           Data.Text (Text)
import qualified Data.Text as T

import           RemoteResources.Management
import           Networks.Relationships
import           Networks.GroupScore
import           Foundation
import           Storage

import           Types

-- | Taxonomic discrimators are the taxonomic levels used to
-- draft the species set and then divide them into groups, respectively.
-- This is a major determinant on game difficulty, and also on the
-- size of database required to generate diversified groups.
generateTaxonomicDiscriminators :: IO TaxonomicDiscriminators
generateTaxonomicDiscriminators = return $ TaxonomicDiscriminators 1 2

generateTips :: TaxonomicDiscriminators -> T.Text
generateTips TaxonomicDiscriminators {..} = "All species belong to the same "
                        <> taxonomicCategories !! rootDiscriminator
                        <> ", while each group has all members in the same "
                        <> taxonomicCategories !! groupDiscriminator
                        <> "."
  where
    taxonomicCategories = [ ""
                          , "Kingdom"
                          , "Phylum"
                          , "Order"
                          , "Genus"
                          , "Family"
                          ]
    fallback_tip        = "No tips, good luck."

-- | Sends a list of species for the game,
-- containing node information for cytoscapejs and
-- ready to be consumed by the frontend.
postDraftSpeciesSimulatorJ :: Handler Value
postDraftSpeciesSimulatorJ = do
  -- TODO: Implement game parameters.
  --parameters <- requireCheckJsonBody :: Handler Types.NewGameRequest
  (group, txd) <- fromMaybe ([], TaxonomicDiscriminators 1 2) <$> liftIO (getGlobalSpeciesGroups 0)
  liftIO        $ putStrLn "Group found."
  -- liftIO      $ print group
  returnJson  $ Types.GameSetup
    { species  = group
    , nbGroups = length $ groupSpeciesByTaxonomy (groupDiscriminator txd) group
    , textTip  = generateTips txd
    , gameTaxonomicDiscriminators = txd
    }

-- | Score the player's categorization.
postValidateGroupsJ :: Handler Value
postValidateGroupsJ = do
  q       <- requireCheckJsonBody :: Handler Types.GameAnswer
  let
    all_species = concat $ Types.speciesGroups q
    TaxonomicDiscriminators {..} = Types.answerTaxonomicDiscriminators q

  records <- liftIO $ mapM loadFromDatabase all_species

  let
    species = catMaybes records

    correct_groups          = groupSpeciesByTaxonomy groupDiscriminator species
    correct_groups_sppnames = extractGroupsSpecies correct_groups
    gs                      = [correct_groups_sppnames, Types.speciesGroups q]
    [gcorrect, gplayer]     = map (simplifyGroups $ concat correct_groups_sppnames) gs

    score = compareSpeciesGroups gcorrect gplayer

  liftIO $ putStrLn $ "Evaluating: " <> show [gcorrect, gplayer]
    <> "\nCorrect: " <> show (head gs)
    <> "\nPlayer: " <> show (last gs)

  returnJson $ Types.GameResult
    { gameResultSuccess       = False
    , gameResultScore         = score
    , gameResultcorrectAnswer = correct_groups_sppnames
    }
  where
    extractGroupsSpecies :: [[Types.RemoteResult]] -> [[Text]]
    extractGroupsSpecies = map $ map (T.toLower . remoteResultScientificName)

-- | Endpoint that when called will trigger species precaching.
-- For ADMIN use only!
getPrecacheGroupsJ :: Handler Value
getPrecacheGroupsJ = do
  liftIO $ putStrLn "Precaching groups..."
  res <- liftIO $ mapM (const $ getGlobalSpeciesGroups 300) [0..30]
  returnJson res

-- | Endpoint to precache groups while not downloading anything.
-- For ADMIN use only!
getPrecacheDiscoverGroupsOnlyJ :: Handler Value
getPrecacheDiscoverGroupsOnlyJ = do
  liftIO $ putStrLn "Precaching groups without new content..."
  res <- liftIO $ mapM (const discoverSpeciesGroups) [0..30]
  returnJson res

discoverSpeciesGroups :: IO (Maybe SpeciesGroup)
discoverSpeciesGroups = do
  image_records <- filter hasImage <$> liftIO retrieveAllDatabaseRecords
  generateTaxonomicDiscriminators >>= getSpeciesGroups image_records 0

-- | Tries to generate groups of species suitable for playing the `Game`.
-- FIXME: The current code organization is not good: this function is huge.
getGlobalSpeciesGroups :: Int -> IO (Maybe SpeciesGroup)
getGlobalSpeciesGroups = \case
  0 -> retrieveStoredGroup
  fetch_remote_number -> do
    putStrLn $ "Creating a new group and this might involve up to  "
      <> show fetch_remote_number
      <> " new downloaded species..."

    fetchRandomSpeciesBatch fetch_remote_number

    td@(TaxonomicDiscriminators rootD gD) <- liftIO generateTaxonomicDiscriminators

    species_records <- liftIO retrieveAllDatabaseRecords
    --mapM_ (print . remoteResultScientificName) recs
    getSpeciesGroups species_records fetch_remote_number td

-- | Fetch species groups from the data.
getSpeciesGroups :: [RemoteResult]
                 -> Int
                 -> TaxonomicDiscriminators
                 -> IO (Maybe SpeciesGroup)
getSpeciesGroups speciesPool fetch_remote_number td@TaxonomicDiscriminators{..} = do
  let
    taxonomic_pools    = groupSpeciesByTaxonomy rootDiscriminator speciesPool

  groups <- mapM (const $ getGroupFromPools taxonomic_pools) [0..30]

  let  substantial_groups = filter (isValidGroupSet td) groups

  putStrLn $ "First round group selection: " <> show substantial_groups
  case substantial_groups of
    [] -> retry [] td
    xs -> do
      --img_groups <- mapM filterSpeciesWithImages xs
      mapM_ (print . length) xs
      putStrLn "Selecting group..."

      -- Select a single group from all the valid that were found.
      selected_group <- choice xs
      case selected_group of
        Just g  -> do
          selected_group_img <- filterSpeciesWithImages $ take 16 g

          putStrLn $ "Group with "
            <> show (length selected_group_img) <> " images."
          if isValidGroupSet td selected_group_img
          then do
            let result = (selected_group_img, td)
            storeGroup result
            return $ Just result
          else getGlobalSpeciesGroups fetch_remote_number
        Nothing -> retry [] td
  where
    retry g discriminators =
      if fetch_remote_number > 0
      then getGlobalSpeciesGroups fetch_remote_number
      else retrieveStoredGroup

    getGroupFromPools taxonomic_pools = do
      taxonomic_pool <- choice taxonomic_pools
      case taxonomic_pool of
        Just pool -> generateSingleGroup pool
        Nothing   -> pure []

-- | Randomically generate a group from a pool of species records.
generateSingleGroup :: [RemoteResult] -> IO [RemoteResult]
generateSingleGroup speciesPool = do
  n <- fromMaybe 4 <$> choice [3..6 :: Int]
  catMaybes <$> mapM (const $ choice speciesPool) [0..n]

-- * Group storage and loading routines.

-- | Ensure a single group seed is properly stored in the database for later use.
storeGroup :: SpeciesGroup -> IO ()
storeGroup (g, td) = do
  existing_groups <- map gameGroupSpecies <$> retrieveAllDatabaseGameSeeds
  if names `elem` existing_groups
  then return ()
  else insertGameSeedInDatabase gameSeed
  where
    names = sort $ map remoteResultScientificName g
    gameSeed = GameGroup names td

-- | Restore a random group seed from the known groups in the database.
retrieveStoredGroup :: IO (Maybe SpeciesGroup)
retrieveStoredGroup = do
  existing_groups <- retrieveAllDatabaseGameSeeds
  selected <- choice existing_groups

  case selected of
    Nothing -> pure Nothing
    Just GameGroup {..} -> do
      records <- catMaybes <$> mapM loadFromDatabase gameGroupSpecies
      when (length records < length gameGroupSpecies)
        $ putStrLn $ "FAIL: Bad group!" <> show gameGroupSpecies
      pure $ Just (records, gameGroupTaxonomicDiscriminators)

-- * Group validation routines.

-- | Check if a group set meets the criteria to be usable in the game.
isValidGroupSet :: TaxonomicDiscriminators -> [Types.RemoteResult] -> Bool
isValidGroupSet TaxonomicDiscriminators {..} groupSet = all (==True)
  [ length groupSet >= 4
  -- Ensure a minimum species set.
  , not $ all (==1) groupSizes
  -- Ensure we have more species than groups.
  , (\l -> l >= 2 && l <= 5) $ length expectedAnswer
  -- Ensure we have between two and five groups.
  ]
  where
    expectedAnswer = groupSpeciesByTaxonomy groupDiscriminator groupSet
    groupSizes     = map length expectedAnswer

-- | Get only species that have available images.
filterSpeciesWithImages :: [Types.RemoteResult] -> IO [Types.RemoteResult]
filterSpeciesWithImages r =
  filter hasImage <$> mapM (upgradeRecord (True, False)) r

-- | Checks whether a record has attached images.
hasImage :: RemoteResult -> Bool
hasImage remote_result =
  case Types.remoteResultImages remote_result of
    Retrieved _  -> True
    NotAvailable -> False
    NeverTried   -> False

-- | Simplify clustering representation. 
-- The output is a label representation for each individual,
-- with the same ordering as the provided reference.
simplifyGroups :: [Text] -> [[Text]] -> String
simplifyGroups reference groups = map findOnGroups reference
  where
    findOnGroups ind = 
      case elemIndex True $ map (\g -> ind `elem` g) groups of 
        Just idx -> labels !! idx
        Nothing  -> 'z'
    labels = ['0'..'z']

-- | Compares a correct species grouping to an to be evaluated group set,
-- Returning a score on correctness, based on the "Mutual Information" algorithm.
-- The score is a float in range 0.0~1.0.
compareSpeciesGroups :: String -> String -> Double
compareSpeciesGroups correct evaluated = player_score / best_score
  where
    calculateScore = mi . counts correct
    player_score   = calculateScore evaluated
    best_score     = calculateScore correct
