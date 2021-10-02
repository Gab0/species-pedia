{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text
import Data.Aeson


data SpeciesQuery = SpeciesQuery
  { queryContent  :: Text
  }
  deriving Show

data RemoteResult = RemoteResult [SpeciesInformation]
  deriving Show
data SpeciesInformation = SpeciesInformation
  { speciesKingdom :: Maybe Text
  , speciesPhylum  :: Maybe Text
  , speciesOrder   :: Maybe Text
  , speciesGenus   :: Maybe Text
  , speciesFamily  :: Maybe Text
  }
  deriving Show
instance FromJSON RemoteResult where
  parseJSON (Object v) = RemoteResult <$> v .: "results"

instance FromJSON SpeciesInformation where
  parseJSON (Object v) =
    SpeciesInformation
      <$> v .:? "kingdom"
      <*> v .:? "phylum"
      <*> v .:? "order"
      <*> v .:? "genus"
      <*> v .:? "family"
