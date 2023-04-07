{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs #-}

module Foundation where

import           System.Metrics.Prometheus.Registry
import           Yesod
import           Yesod.Static
import           Metrics
-- Define URL Routes.
data App = App
 { getStatic :: Static
 , prometheusState :: PrometheusState
 }

mkYesodData "App" [parseRoutes|
/ DatabaseInformationJ GET
-- ^ Show statistcs on database contents;
/react        HomeReactR GET
-- ^ React homepage;
/search       SearchR POST
-- ^ Main search results page;
/search.json  SearchJ POST
-- ^ Search results as json;
/search_react SearchReactR POST
-- ^ Search results as react;
/static       StaticR Static getStatic
-- ^ Static file directory;
/favicon.ico  FaviconR GET
-- ^ Serve the website's icon;

/game/new      DraftSpeciesSimulatorJ POST
-- ^ Ask for a new species list;
/game/answer   ValidateGroupsJ POST
-- ^ Validate species groups as done by some human friend;

/database/precache PrecacheGroupsJ GET
-- ^ Fetch and build species groups for later use in the game;
/database/buildgroups PrecacheDiscoverGroupsOnlyJ GET
-- ^ Build species groups based on known species for later use in the game;

/metrics PrometheusMetricsR GET
-- ^ Prometheus metrics
|]

instance Yesod App
