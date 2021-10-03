{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text
import Data.Aeson


data SpeciesQuery = SpeciesQuery
  { queryContent  :: Text
  , jsonResponse  :: Bool
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
  , vernacularNames :: [VernacularName]
  }
  deriving Show

newtype VernacularName = VernacularName Text
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
      <*> v .:? "vernacularNames" .!= []
  parseJSON _         = fail ""

instance FromJSON VernacularName where
  parseJSON (Object v) =
    VernacularName <$> v .: "vernacularName"
