{-# LANGUAGE OverloadedStrings          #-}

module Game where

import           Yesod
import           Yesod.Form

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
  group <- liftIO getSpeciesGroup
  liftIO $ print group
  returnJson  $ Types.GameSetup group "No tips, good luck."

-- | Validate species groups as classified by the player.
postValidateGroupsJ :: Handler Value
postValidateGroupsJ = do
  q <- requireCheckJsonBody :: Handler Types.GameAnswer
  returnJson $ Types.GameResult False []


getSpeciesGroup :: IO [Types.RemoteResult]
getSpeciesGroup = do
  k <- liftIO $ fetchRandomSpeciesBatch 100
  liftIO $ insertInDatabaseBatch k

  recs <- liftIO retrieveGroupFromDatabase
  let
    groups             = groupSpeciesByTaxonomy 2 recs
    substantial_groups = filter (\g -> length g >= 5) groups

  case substantial_groups of
    []   -> getSpeciesGroup
    xs   -> choice xs
