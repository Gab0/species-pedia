{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage where

import           System.Random

import           Data.Maybe
import qualified Data.Text as T

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Control.Monad.IO.Class
import           Types

databaseFilepath :: T.Text
databaseFilepath = "species-db.sqlite" -- could be ":memory:" for non-persistent db.

-- | Create a new database.
initializeDatabase :: IO ()
initializeDatabase =
  runSqlite databaseFilepath $ runMigration migrateAll

-- | Load a record that matches a string from the database.
loadFromDatabase :: T.Text -> IO (Maybe RemoteResult)
loadFromDatabase search_query =
  runSqlite databaseFilepath $ do
    record <- getBy
            $ Types.QueryString search_query

    return $  entityVal
          <$> record

-- | Insert a record in the database.
insertInDatabase :: Types.RemoteResult -> IO ()
insertInDatabase n =
  runSqlite databaseFilepath $ do
    existent <- getBy $ Types.QueryString $ remoteResultScientificName n
    case existent of
      Just k  -> replace (entityKey k) n
      Nothing -> insert_ n

-- | Insert multiple records in the database.
insertInDatabaseBatch :: [Types.RemoteResult] -> IO ()
insertInDatabaseBatch = mapM_ insertInDatabase

-- | Retrieve all records from the database.
retrieveGroupFromDatabase :: IO [RemoteResult]
retrieveGroupFromDatabase =
  runSqlite databaseFilepath $ do
    (records :: [RemoteResult]) <-  map entityVal
                                <$> selectList [] []
    return records

-- | Retrieve n randomly-selected `RemoteResult` elements.
draftFromDatabase :: Int -> IO [RemoteResult]
draftFromDatabase num = do
  records  <- retrieveGroupFromDatabase
  sequence  $ replicate num (choice records)

-- | Select a random element from a list.
choice :: [a] -> IO a
choice r = (!!) r <$> randomRIO (0, length r - 1)
