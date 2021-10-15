{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}

module Types where

import           Data.Text
import           Data.Aeson
import           Data.Aeson.TH
import           Database.Persist
import           Database.Persist.TH
import           Language.Haskell.TH
import qualified Data.ByteString.Lazy.UTF8 as LUTF8

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
    images [String]
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
                      <*> return []
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

-- Here we declare ToJSONS default functions
-- with the help of Aeson.TH.
-- Data.Persistent offers us free ToJSON and FromJSON,
-- but those would only come together.
-- We want to have custom FromJSONs,
-- to retain some control over remote content parsing.
$(deriveToJSON defaultOptions ''VernacularName)
$(deriveToJSON defaultOptions ''SpeciesInformation)
$(deriveToJSON defaultOptions ''RemoteResult)

-- $(deriveToJSON defaultOptions ''FormResult SpeciesQuery)
