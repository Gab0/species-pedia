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
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}

module Types where

import           Data.Text
import           Data.Aeson
import           Data.Aeson.TH
import           Database.Persist
import           Database.Persist.TH
import           Language.Haskell.TH
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import           Generics.Deriving.Semigroup
import           GHC.Generics (Generic)

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
    deriving Eq
VernacularName
    vernacularName Text
    deriving Show
    deriving Eq
|]

data SpeciesQuery = SpeciesQuery
  { queryContent  :: Text
  , jsonResponse  :: Bool
  }
  deriving Show

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

-- So, not going this way:
-- $(deriveToJSON defaultOptions ''FormResult SpeciesQuery)

-- | Semigroup instance is used to combine two
-- data objects.
-- FIXME: May improve the organization on this.
instance Semigroup SpeciesInformation where
   (SpeciesInformation k0 p0 o0 g0 f0 ts0 vn0) <> (SpeciesInformation k1 p1 o1 g1 f1 ts1 vn1) =
     SpeciesInformation (maybeText k0 k1) (maybeText p0 p1) (maybeText o0 o1) (maybeText g0 g1) (maybeText f0 f1) (ts0 <> ts1) (vn0 <> vn1)
    where
      maybeText (Just a0) (Just a1) = Just
                                    $ manageVariations a0 a1
      maybeText a b                 = a <> b

      manageVariations :: Text -> Text -> Text
      manageVariations t0 t1
        | t0 == t1  = t0
        | otherwise = case t1 `elem` content_list of
            True  -> t0
            False -> intercalate separator $ t1 : content_list
          where
            content_list = splitOn separator t0
            separator    = " | "
