{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage where

import           Control.Monad.Trans.Reader ( ReaderT (..) )
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Resource.Internal

import           Data.ByteString ( ByteString )
import           Data.Maybe ( catMaybes )
import qualified Data.Text as T

import           System.Random
import           System.Environment.Blank

import Database.Persist
    ( selectList,
      Entity(entityKey, entityVal),
      PersistStoreWrite(insert_, replace),
      PersistUniqueRead(getBy) )

import           Database.Persist.Postgresql
import           Types

-- | PostgreSQL connection string.
databaseConnection :: ByteString
databaseConnection = "host=speciespedia-database port=5432 user=postgres dbname=postgres"

-- | The filepath could be ":memory:" for non-persistent db.
-- FIXME: SQLite is deprecated. Should be support it along with PostgreSQL?
getDatabaseFilepath :: IO T.Text
getDatabaseFilepath = T.pack <$> getEnvDefault "DATABASE_FILEPATH" "species-db.sqlite"

-- | Create a new database.
initializeDatabase :: IO ()
initializeDatabase = runDB $ runMigration migrateAll

-- | Load a record that matches a string from the database.
loadFromDatabase :: T.Text -> IO (Maybe RemoteResult)
loadFromDatabase search_query = runDB $ do
    record <- getBy
            $ Types.QueryString search_query

    return $ entityVal <$> record

-- | Run a database action.
runDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) b -> IO b
runDB = runResourceT . runNoLoggingT . withPostgresqlConn databaseConnection . runReaderT

-- | Insert a record in the database.
insertInDatabase :: Types.RemoteResult -> IO ()
insertInDatabase n = runDB $ do
  existent <- getBy $ Types.QueryString $ remoteResultScientificName n
  case existent of
    Just k  -> replace (entityKey k) n
    Nothing -> insert_ n

-- | Insert multiple records in the database.
insertInDatabaseBatch :: [Types.RemoteResult] -> IO ()
insertInDatabaseBatch = mapM_ insertInDatabase

-- | Retrieve all records from the database.
retrieveAllDatabaseRecords :: IO [RemoteResult]
retrieveAllDatabaseRecords = runDB $ do
    (records :: [RemoteResult]) <-  map entityVal
                                <$> selectList [] []
    return records

-- | Retrieve all game seeds from the database.
retrieveAllDatabaseGameSeeds :: IO [GameGroup]
retrieveAllDatabaseGameSeeds = runDB $ do
    (records :: [GameGroup]) <- map entityVal <$> selectList [] []
    return records

-- | Insert a GameSeed object in the database.
insertGameSeedInDatabase :: GameGroup -> IO ()
insertGameSeedInDatabase = runDB . insert_

-- | Retrieve n randomly-selected `RemoteResult` elements.
draftFromDatabase :: Int -> IO [RemoteResult]
draftFromDatabase num = do
  records   <- retrieveAllDatabaseRecords
  retrieved <- sequence $ replicate num (choice records)
  return     $ catMaybes retrieved

-- | Select a random element from a list.
choice :: [a] -> IO (Maybe a)
choice [] = return Nothing
choice r  = Just . (!!) r <$> randomRIO (0, length r - 1)
