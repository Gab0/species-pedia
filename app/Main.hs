{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE DataKinds             #-}

module Main where

import           System.IO

import           Yesod
import           Yesod.Static

import           Network.Wai.Middleware.Cors
import           Network.Wai.Handler.Warp (run)

import           Data.Text ( Text )
import           Data.Proxy
import           Data.Aeson.TypeScript.TH

import           Debug
import           Foundation
import           Frontend
import           Game
import           Metrics
import           Storage
import           Types

-- Is it worth adding wai-3.2.3 to the project just for this type signature?
--allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
    simpleCorsResourcePolicy
        { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
        , corsRequestHeaders = ["Authorization", "Content-Type"]
        }


generateTSBindings :: String
generateTSBindings = formatTSDeclarations (
  getTypeScriptDeclarations (Proxy :: Proxy VernacularName) <>
  getTypeScriptDeclarations (Proxy :: Proxy SpeciesInformation) <>
  getTypeScriptDeclarations (Proxy :: Proxy RemoteResult) <>
  getTypeScriptDeclarations (Proxy :: Proxy GameAnswer) <>
  getTypeScriptDeclarations (Proxy :: Proxy GameSetup) <>
  getTypeScriptDeclarations (Proxy :: Proxy GameResult) <>
  getTypeScriptDeclarations (Proxy :: Proxy NewGameRequest) <>
  getTypeScriptDeclarations (Proxy :: Proxy (RemoteContent T)) <>
  getTypeScriptDeclarations (Proxy :: Proxy TaxonomicDiscriminators)
  )

writeTSBindings :: IO ()
writeTSBindings =
  writeFile "Types.ts" generateTSBindings


mkYesodDispatch "App" resourcesApp

-- | Create and launch the application,
-- and create the TypeScript bindings.
main :: IO ()
main = do
  initializeDatabase
  putStrLn $ spacer
          ++ "Serving application on port "
          ++ show port
          ++ "."
          ++ spacer
  static@(Static settings) <- static "static/"
  writeTSBindings

  prometheusState <- initializePrometheusState

  -- Convert the Yesod website into a WAI Application.
  app     <- toWaiAppPlain $ App static prometheusState
  run port $ allowCors app
  where
    port   = 5000
    spacer = take 4 $ repeat '\n'
