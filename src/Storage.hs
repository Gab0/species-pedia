{-# LANGUAGE OverloadedStrings #-}

module Storage where

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import qualified Data.Text as T
import           Types

databaseFilepath = "species-db.sqlite"
-- runDB :: IO ()
-- runDB = runSqlite ":memory:"


initializeDatabase :: IO ()
initializeDatabase =
  runSqlite databaseFilepath $ runMigration migrateAll

  
loadFromDatabase :: T.Text -> IO (Maybe RemoteResult)
loadFromDatabase search_query =
  runSqlite databaseFilepath $ do
    w <- getBy
       $ Types.QueryString search_query

    return $  entityVal
          <$> w



insertInDatabase :: Types.RemoteResult -> IO (Key RemoteResult)
insertInDatabase n =
  runSqlite databaseFilepath $ insert n
