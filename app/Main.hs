{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}

module Main where


import           Yesod
import           Yesod.Static

import           Network.Wai.Middleware.Cors
import           Network.Wai.Handler.Warp (run)

import           Frontend
import           Storage

allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
    simpleCorsResourcePolicy
        { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
        , corsRequestHeaders = ["Authorization", "Content-Type"]
        }

main :: IO ()
main = do
  initializeDatabase
  putStrLn $ spacer
          ++ "Serving application on port "
          ++ show port
          ++ "."
          ++ spacer
  static@(Static settings) <- static "static/"

  -- Convert the Yesod website into a WAI Application.
  app     <- toWaiAppPlain $ App static
  run port $ allowCors app
  where
    port   = 5000
    spacer = take 4 $ repeat '\n'
