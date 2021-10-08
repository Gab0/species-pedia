{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import           Data.Text
import           Data.Aeson
import           Database.Persist
import           Database.Persist.TH

-- Declare the datatypes inside this TemplateHaskell quasi-quoter.
-- TH creates the data types and derivings to allow the usage
-- of these datatypes with the database.
--
-- For example, in order to get the kingdom of a SpeciesInformation,
-- the getter that is created is speciesInformationKingdom.
--
-- QueryString is a Unique constraint field for our RemoteResult type.
-- Everything is explained here:
-- https://www.yesodweb.com/book/persistent
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
RemoteResult
    originalQuery Text
    information [SpeciesInformation]
    QueryString originalQuery
    deriving Show
SpeciesInformation
    kingdom Text Maybe
    phylum Text Maybe
    order Text Maybe
    genus Text Maybe
    family Text Maybe
    statuses [Text]
    vernacularNames [VernacularName]
    deriving Show
VernacularName
    vernacularName Text
    deriving Show
|]

data SpeciesQuery = SpeciesQuery
  { queryContent  :: Text
  , jsonResponse  :: Bool
  }
  deriving Show

-- newtype RemoteResult = RemoteResult [SpeciesInformation]
--   deriving Show

-- data SpeciesInformation = SpeciesInformation
--   { speciesKingdom :: Maybe Text
--   , speciesPhylum  :: Maybe Text
--   , speciesOrder   :: Maybe Text
--   , speciesGenus   :: Maybe Text
--   , speciesFamily  :: Maybe Text
--   , threatStatuses :: [Text]
--   , vernacularNames :: [VernacularName]
--   }
--   deriving Show

--newtype VernacularName = VernacularName Text
--  deriving Show


instance FromJSON RemoteResult where
  parseJSON (Object v) =  RemoteResult
                      <$> return ""
                      <*> v .: "results"
  parseJSON _          =  fail ""

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
  parseJSON _          = fail ""
