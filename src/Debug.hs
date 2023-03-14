 
module Debug where

import Yesod

import Foundation
import Storage
import Types

getDatabaseInformationJ :: HandlerFor App Value
getDatabaseInformationJ = do
  all_records <- liftIO retrieveAllDatabaseRecords
  let
    dbsize = length all_records
    nb_pictured = length $ filter hasImages all_records

  returnJson $ DatabaseDebugInformation dbsize nb_pictured 0
  where
    hasImages record =
      case remoteResultImages record of
        Retrieved [_] -> True
        _             -> False
