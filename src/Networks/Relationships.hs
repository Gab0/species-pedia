

module Networks.Relationships where

import Data.List
import RemoteResources.GBIF
import Types

groupSpeciesByTaxonomy :: Int -> [RemoteResult] -> [[RemoteResult]]
groupSpeciesByTaxonomy level = groupBy (matchSpecies level)

-- | Determines if two species belong to the same group.
-- The criteria has one parameter: an integer called 'Taxonomic Discrimination Level' (made up here).
-- Considering Kingdom, Phylum, etc... this integer points to the taxonomic level where
-- two species must be equal to be considered in the same category.
matchSpecies :: Int                -- ^ Taxonomic Discrimination Level
             -> RemoteResult       -- ^ Species A
             -> RemoteResult       -- ^ Species B
             -> Bool               -- ^ Same group?
matchSpecies tdl s0 s1 = all (==True)
                       $ map evaluate fieldGetters
  where
    fieldGetters         = take tdl genusFields
    evaluate (_, getter) =
      case length $ nub $ map (getter . remoteResultInformation) [s0, s1] of
        1 -> True
        _ -> False
