 
module Debug where

import Yesod

import Foundation
import Game
import Storage
import Types

getDatabaseInformationJ :: HandlerFor App Value
getDatabaseInformationJ = do
  all_records <- liftIO retrieveAllDatabaseRecords
  let
    dbsize = length all_records
    nb_pictured = length $ filter hasImage all_records

  nb_groups <- length <$> liftIO retrieveAllDatabaseGameSeeds
  returnJson $ DatabaseDebugInformation dbsize nb_pictured nb_groups
