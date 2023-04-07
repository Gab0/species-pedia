{-# LANGUAGE RecordWildCards #-}
module Debug where

import Data.ByteString.Lazy ( ByteString )

import Yesod

import Foundation
import Game
import Metrics
import Storage
import Types ( DatabaseDebugInformation(DatabaseDebugInformation) )

import System.Metrics.Prometheus.Encode.Text
import Data.ByteString.Builder (toLazyByteString)


getDatabaseInformationJ :: HandlerFor App Value
getDatabaseInformationJ = do
  all_records <- liftIO retrieveAllDatabaseRecords
  let
    dbsize = length all_records
    nb_pictured = length $ filter hasImage all_records

  nb_groups <- length <$> liftIO retrieveAllDatabaseGameSeeds
  returnJson $ DatabaseDebugInformation dbsize nb_pictured nb_groups

-- | Get prometheus metrics;
getPrometheusMetricsR :: Handler TypedContent
getPrometheusMetricsR = do
  App {..} <- getYesod
  res <- toLazyByteString . encodeMetrics <$> liftIO (prometheusSampler prometheusState)
  return $ TypedContent typeOctet $ toContent (res :: ByteString)
