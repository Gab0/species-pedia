
module Game where

import           Yesod
import           Yesod.Form
import           Yesod.Core.Handler
import           Data.Aeson
import           Foundation

import           Types


-- | Sends a list of species for the game,
-- containing node information for cytoscapejs and
-- ready to be consumed by the frontend.
postAskForSpeciesJ :: Handler Value
postAskForSpeciesJ = do
  q <- requireCheckJsonBody :: Handler Types.SpeciesQuery
  return $ object []

-- | Validate species groups as classified by the player.
postValidateGroupsJ :: Handler Value
postValidateGroupsJ = do
  return $ object []
