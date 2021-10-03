{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Yesod
import           Yesod.Form
import qualified Data.Text as T
import qualified Types
import           RemoteResources

-- Define URL Routes.
data App = App
mkYesod "App" [parseRoutes|
/ HomeR GET
/search SearchR POST
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

globalHeader :: WidgetFor App ()
globalHeader =
    toWidget [lucius| h1 { color: green; } |] >>
    toWidget [hamlet|
                    <h1> Species Information Retriever </h1>
                    |]

-- Declare the query form.
-- This is better explained here:
-- https://www.yesodweb.com/book/forms#forms_kinds_of_forms
searchForm :: Html -> MForm Handler (FormResult Types.SpeciesQuery, Widget)
searchForm =  renderDivs
           $  Types.SpeciesQuery
          <$> areq textField "Search Query " Nothing

-- Render the page that shows query results
postSearchR :: HandlerFor App Html
postSearchR = do
  ((result, widget), enctype) <- runFormPost searchForm
  let x = evalResult result

  raw_gbif_response <- liftIO
                     $ fetchInformationGBIF
                     $ T.unpack x

  let k = decodeInformationGBIF raw_gbif_response

  defaultLayout $ do
    setTitle "Search Result"
    globalHeader
    [whamlet| <div>You searched for '#{x}'.</div>|]
    [whamlet| <br><br>|]
    [whamlet| <div>|]
    case k of
      Right (Types.RemoteResult info) -> zipWithM_ renderSingleResult [1..] info
      _                               -> [whamlet| Error processing request.|]
  where
    evalResult (FormSuccess f) = Types.queryContent f
    evalResult _               = ""

-- Render information for a single result from GBIF.
renderSingleResult :: Int -> Types.SpeciesInformation -> Widget
renderSingleResult index information = do
  [whamlet| <div> Result <b># #{index}</b>|]
  mapM_ (\(t, g) -> showGenusField t (g information)) genusFields
  [whamlet|<br>|]
  mapM_ (\t -> [whamlet| #{t}|]) $ Types.threatStatuses information
  [whamlet| <br><hr><br>|]

  where
    genusFields :: [(String, Types.SpeciesInformation -> Maybe T.Text)]
    genusFields =
      [ ("Kingdom", Types.speciesKingdom)
      , ("Phylum", Types.speciesPhylum)
      , ("Order", Types.speciesOrder)
      , ("Genus", Types.speciesGenus)
      , ("Family", Types.speciesFamily)
      ]

showGenusField :: String -> Maybe T.Text -> Widget
showGenusField name value =
  [whamlet| <div class='#{name}'>#{name}: #{n}</div>|]
  where
    n = case value of
      Just v -> v
      Nothing -> "-"

-- This function renders the main page, which is also the search page.
-- 'julius' templates are for javascript code.
-- 'lucius' templates are for CSS style code.
-- 'hamlet' templates are for HTML code.
-- 'whamlet' temaplates are the same as 'hamlet',
--   but they can show widgets with #{}
--   and they are already Widgets themselves
getHomeR :: HandlerFor App Html
getHomeR = do
    (widget, enctype) <- generateFormPost searchForm
    defaultLayout $ do
      setTitle "Species Searcher."

      globalHeader
      toWidgetHead
        [hamlet|
            <meta name=keywords content="species information">
        |]
      [whamlet|<form method=post action=@{SearchR} enctype=#{enctype}>
              ^{widget}
            <button>Search</button>
            |]

      [whamlet|This application searches for information about species in the internet.<br>
               Query for a species name such as 'Felis catus'.|]


main :: IO ()
main = do
  putStrLn $ spacer
          ++ "Serving application on port "
          ++ show port
          ++ "."
          ++ spacer
  warp port App
  where
    port = 3000
    spacer = take 4 $ repeat '\n'
