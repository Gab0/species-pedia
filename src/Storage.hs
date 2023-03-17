{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage where

import           System.Random
import           System.Environment.Blank
import           Data.Maybe
import qualified Data.Text as T

import           Database.Persist
import           Database.Persist.Sqlite
import           Types

-- | The filepath could be ":memory:" for non-persistent db.
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
--runDB :: (a -> IO b) -> IO ()
runDB :: transformers-0.5.6.2:Control.Monad.Trans.Reader.ReaderT
  SqlBackend
  (monad-logger-0.3.36:Control.Monad.Logger.NoLoggingT
     (Control.Monad.Trans.Resource.Internal.ResourceT IO))
  b
-> IO b
runDB f = do
  databaseFilepath <- getDatabaseFilepath
  runSqlite databaseFilepath f

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
