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
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE InstanceSigs               #-}

module Types where

import           Data.Text    (Text)
import qualified Data.Text as T
import           Data.Aeson
import           Data.Aeson.TH
import           Database.Persist
import           Data.Default
import           Database.Persist.TH
import           Data.Aeson.TypeScript.TH
import           Data.Proxy
import Database.Persist.Sql

import           Language.Haskell.TH
import           Text.Read

import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import Generics.Deriving.Semigroup ()
import           GHC.Generics (Generic)

data TaxonomicDiscriminators = TaxonomicDiscriminators
  { rootDisciminator   :: !Int
  , groupDiscriminator :: !Int
  } deriving (Show, Read, Generic)

$(deriveToJSON defaultOptions ''TaxonomicDiscriminators)
$(deriveFromJSON defaultOptions ''TaxonomicDiscriminators)
$(deriveTypeScript defaultOptions ''TaxonomicDiscriminators)

instance PersistField TaxonomicDiscriminators where
  toPersistValue TaxonomicDiscriminators {..} = toPersistValue $ show (rootDisciminator, groupDiscriminator)
  fromPersistValue :: PersistValue -> Either Text TaxonomicDiscriminators
  fromPersistValue (PersistText pv) =
    case readMaybe $ T.unpack pv of
      Just (a, b) -> Right $ TaxonomicDiscriminators a b
      Nothing     -> Left $ "Unable to parse " <> pv <> "."
  fromPersistValue e = Left $ "Bad input value of" <> T.pack (show e)

instance PersistFieldSql TaxonomicDiscriminators where
  sqlType _ = SqlString

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
    scientificName Text
    information SpeciesInformation
    images (RemoteContent [String])
    wikipedia (RemoteContent Text)
    QueryString scientificName
    deriving Show
    deriving Eq
SpeciesInformation
    kingdom Text Maybe
    phylum Text Maybe
    order Text Maybe
    genus Text Maybe
    family Text Maybe
    statuses [Text]
    scientificName Text
    vernacularNames [VernacularName]
    deriving Show
    deriving Eq
VernacularName
    vernacularName Text
    deriving Show
    deriving Eq
GameGroup
    species [Text]
    taxonomicDiscriminators TaxonomicDiscriminators
|]

data SpeciesQuery = SpeciesQuery
  { queryContent  :: !Text
  , jsonResponse  :: !Bool
  }
  deriving Show

instance Default SpeciesInformation where
  def = SpeciesInformation Nothing Nothing Nothing Nothing Nothing [] "" []

instance Default RemoteResult where
  def = RemoteResult "" "" def NeverTried NeverTried

-- | Encapsulates retrievable remote content to avoid querying
-- unavailable content more than once.
data RemoteContent a = Retrieved a
                     | NotAvailable
                     | NeverTried
  deriving (Eq, Show, Read)

instance (Show a, Read a) => PersistField (RemoteContent a) where
    toPersistValue = toPersistValue . show
    fromPersistValue (PersistText pv) =
        case readMaybe $ T.unpack pv of
            Just w  -> Right w
            Nothing -> Left $ T.pack $ "Unable to parse RemoteContent: <" <> show pv <> ">"
    fromPersistValue _                = Left "Weird RemoteContent result."

instance (Show a, Read a) => PersistFieldSql (RemoteContent a) where
    sqlType :: (Show a, Read a) => Proxy (RemoteContent a) -> SqlType
    sqlType _ = SqlString

-- | Stores relevant information from a direct fetch from GBIF.
-- These are retrieved by using numerical ID.
-- FIXME: Deprecated?
data GBIFFetchResult = GBIFFetchResult
  { fetchRank           :: !T.Text
  , fetchScientificName :: !T.Text
  }
  deriving (Show, Eq)

instance FromJSON GBIFFetchResult where
  parseJSON (Object v) = GBIFFetchResult
                      <$> v .: "rank"
                      <*> v .: "scientificName"
  parseJSON _          = fail "Invalid GBIF result."

-- | Stores a species search result from GBIF.
newtype GBIFSearchResult = GBIFSearchResult [SpeciesInformation]
  deriving (Show, Eq)

instance FromJSON GBIFSearchResult where
  parseJSON (Object v) =  GBIFSearchResult
                      <$> v .: "results"
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
      <*> v .:  "scientificName"
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

$(deriveFromJSON defaultOptions ''SpeciesQuery)
$(deriveToJSON defaultOptions   ''VernacularName)
$(deriveToJSON defaultOptions   ''SpeciesInformation)
$(deriveToJSON defaultOptions   ''RemoteResult)
$(deriveToJSON defaultOptions   ''RemoteContent)

-- So, not going this way:
-- $(deriveToJSON defaultOptions ''FormResult SpeciesQuery)


-- | TypeScript derivings to generate types descriptors
-- for the frontend.
$(deriveTypeScript defaultOptions ''SpeciesQuery)
$(deriveTypeScript defaultOptions ''VernacularName)
$(deriveTypeScript defaultOptions ''SpeciesInformation)
$(deriveTypeScript defaultOptions ''RemoteResult)
$(deriveTypeScript defaultOptions ''RemoteContent)


-- | Semigroup instance is used to combine two
-- SpeciesInformation objects.
-- FIXME: May improve the organization on this.
instance Semigroup SpeciesInformation where
   (SpeciesInformation k0 p0 o0 g0 f0 ts0 sn0 vn0) <> (SpeciesInformation k1 p1 o1 g1 f1 ts1 sn1 vn1) =
     SpeciesInformation (maybeText k0 k1) (maybeText p0 p1) (maybeText o0 o1) (maybeText g0 g1) (maybeText f0 f1) (ts0 <> ts1) sn0 (vn0 <> vn1)
    where
      maybeText (Just a0) (Just a1) = Just
                                    $ combineVariations a0 a1
      maybeText a b                 = a <> b

      combineVariations :: Text -> Text -> Text
      combineVariations t0 t1
        | t0 == t1  = t0
        | otherwise =
          if t1 `elem` content_list
          then t0
          else T.intercalate separator $ t1 : content_list
          where
            content_list = T.splitOn separator t0
            separator    = " | "

-- | Parameters that define a game session.
-- (GAME STEP 1: Client sents this to the server.)
-- TODO: Most parameter effects are yet to be implemented.
data NewGameRequest = NewGameRequest
  { speciesNumber  :: !Int
  , groupNumber    :: !Int
  , speciesGroup   :: !Int
  , gameDifficulty :: !Int
  }

$(deriveFromJSON defaultOptions ''NewGameRequest)

-- | Information required to set a game up.
-- (GAME STEP 2: Server sends this to the client.)
data GameSetup = GameSetup
  { species                     :: ![RemoteResult]
  , nbGroups                    :: !Int
  , textTip                     :: !Text
  , gameTaxonomicDiscriminators :: !TaxonomicDiscriminators
  }

$(deriveToJSON defaultOptions ''GameSetup)

-- | How the player organized the species.
-- (GAME STEP 3: Client sents this to the server.)
data GameAnswer = GameAnswer
  { speciesGroups                 :: ![[Text]]
  , answerTaxonomicDiscriminators :: !TaxonomicDiscriminators
  }

$(deriveFromJSON defaultOptions ''GameAnswer)

-- | Contains the result of a game.
-- (GAME STEP 4: Server sends scores to the client: Game Over, well done (maybe)!)
data GameResult = GameResult
  { gameResultSuccess       :: !Bool
  , gameResultScore         :: !Double
  , gameResultcorrectAnswer :: ![[Text]]
  }

$(deriveToJSON defaultOptions ''GameResult)

$(deriveTypeScript defaultOptions ''NewGameRequest)
$(deriveTypeScript defaultOptions ''GameSetup)
$(deriveTypeScript defaultOptions ''GameAnswer)
$(deriveTypeScript defaultOptions ''GameResult)

data DatabaseDebugInformation = DatabaseDebugInformation
  { databaseDebugNumberOfSpecies  :: !Int
  , databaseDebugNumberOfPictured :: !Int
  , databaseDebugNumberOfGroups   :: !Int
  }

$(deriveToJSON defaultOptions ''DatabaseDebugInformation)

