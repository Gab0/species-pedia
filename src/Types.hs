{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text
import Data.Aeson


data SpeciesQuery = SpeciesQuery
  { queryContent  :: Text
  }
  deriving Show

newtype RemoteResult = RemoteResult [SpeciesInformation]
  deriving Show

data SpeciesInformation = SpeciesInformation
  { speciesKingdom :: Maybe Text
  , speciesPhylum  :: Maybe Text
  , speciesOrder   :: Maybe Text
  , speciesGenus   :: Maybe Text
  , speciesFamily  :: Maybe Text
  , threatStatuses :: [Text]
  }
  deriving Show
instance FromJSON RemoteResult where
  parseJSON (Object v) = RemoteResult <$> v .: "results"
  parseJSON _          = fail ""

instance FromJSON SpeciesInformation where
  parseJSON (Object v) =
    SpeciesInformation
      <$> v .:? "kingdom"
      <*> v .:? "phylum"
      <*> v .:? "order"
      <*> v .:? "genus"
      <*> v .:? "family"
      <*> v .:? "threatStatuses" .!= []
  parseJSON _         = fail ""
