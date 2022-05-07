{-# LANGUAGE OverloadedStrings          #-}

module Game where

import           Yesod
import           Yesod.Form

import           Control.Monad

import           RemoteResources.Management
import           Networks.Relationships
import           Foundation
import           Storage

import           Types

-- | Sends a list of species for the game,
-- containing node information for cytoscapejs and
-- ready to be consumed by the frontend.
postAskForSpeciesJ :: Handler Value
postAskForSpeciesJ = do
  -- TODO: Implement game parameters.
  --parameters <- requireCheckJsonBody :: Handler Types.NewGameRequest
  base_group <- liftIO $ getSpeciesGroup False
  liftIO      $ putStrLn "Group found."
  group      <- liftIO $ upgradeRecordsIfRequired base_group
  liftIO      $ putStrLn "Group information retrieved."
  -- liftIO      $ print group
  returnJson  $ Types.GameSetup group "No tips, good luck."

-- | Validate species groups as classified by the player.
postValidateGroupsJ :: Handler Value
postValidateGroupsJ = do
  q <- requireCheckJsonBody :: Handler Types.GameAnswer
  returnJson $ Types.GameResult False []

postPrecacheGroupsJ :: Handler Value
postPrecacheGroupsJ = do
  res <- liftIO $ getSpeciesGroup True
  returnJson res

-- | Tries to generate groups of species suitable for playing the `Game`.
-- FIXME: The current code organization is not good... e.g. this function is huge.
getSpeciesGroup :: Bool -> IO [Types.RemoteResult]
getSpeciesGroup fetch_remote = do
  putStrLn $ "Fetching group of " <> show number <> " species..."

  when fetch_remote $ do
    k <- liftIO $ fetchRandomSpeciesBatch number
    liftIO $ insertInDatabaseBatch k

  recs <- liftIO retrieveGroupFromDatabase
  --mapM_ (print . remoteResultScientificName) recs
  let
    groups             = groupSpeciesByTaxonomy 2 recs
    substantial_groups = filter isValidGroup groups

  putStrLn $ "First round group selection: " <> show substantial_groups
  case substantial_groups of
    []   -> retry []
    xs   -> do
      --img_groups <- mapM filterSpeciesWithImages xs
      mapM_ (print . length) xs
      putStrLn "Selecting group..."
      selected_group <- choice $ filter isValidGroup xs
      case selected_group of
        Just g  -> do
          selected_group_img <- filterSpeciesWithImages $ take 16 g
          putStrLn $ "Group with " <> show (length selected_group_img) <> " images."
          case isValidGroup selected_group_img of
            True  -> return g
            False -> retry g
        Nothing -> retry []
  where
    number         = 100
    isValidGroup g = length g >= 5
    retry g        =
      case fetch_remote of
        False -> return g
        True  -> getSpeciesGroup fetch_remote

-- evaluateGroup :: [Types.RemoteResult] -> IO [Types.RemoteResult]
-- evaluateGroup




-- | Get only species that have available images.
filterSpeciesWithImages :: [Types.RemoteResult] -> IO [Types.RemoteResult]
filterSpeciesWithImages r =
  filter checkImage <$> mapM (upgradeRecord (True, False)) r
  where
    checkImage remote_result =
      case Types.remoteResultImages remote_result of
        Retrieved _ -> True
        _           -> False
