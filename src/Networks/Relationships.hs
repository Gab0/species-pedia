

module Networks.Relationships where

import Types

groupSpecies :: [SpeciesInformation] -> [[SpeciesInformation]]
groupSpecies group = groupBy (compare `on`)


matchSpecies :: SpeciesInformation -> SpeciesInformation -> Bool
