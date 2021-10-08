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
import           Data.Either
import qualified Types
import           RemoteResources
import           Storage

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
          <*> areq hiddenField "" (Just False)

-- | Manage remote content retrieval and the cache routines.
--   Fetch, insert to DB or retrieve from DB depending on
--   DB data availability.
manageCachedRemoteContent :: T.Text -> IO (Either String Types.RemoteResult)
manageCachedRemoteContent query_string = do
  cached_response   <- loadFromDatabase query_string

  case cached_response of
    Just response -> return
                   $ Right response
    Nothing       -> do
      information <- decodeInformationGBIF
        <$> fetchInformationGBIF (T.unpack query_string)

      case information of
        Left a  -> return $ Left a
        Right (Types.RemoteResult _ content) -> do
          k <- insertInDatabase
             $ Types.RemoteResult query_string content
          return information


-- Render the page that shows query results
postSearchR :: HandlerFor App Html
postSearchR = do
  ((result, widget), enctype) <- runFormPost searchForm
  let query_string = evalResult result

  content <- liftIO
           $ manageCachedRemoteContent query_string

  defaultLayout $ do
    setTitle "Search Result"
    globalHeader
    [whamlet| <div>You searched for '#{query_string}'.</div>|]
    [whamlet| <br><br>|]
    [whamlet| <div>|]
    case content of
      Right (Types.RemoteResult _ results) -> zipWithM_ renderSingleResult [1..] results
      _                                    -> [whamlet| Error processing request.|]
  where
    evalResult (FormSuccess f) = Types.queryContent f
    evalResult _               = ""

-- Render information for a single result from GBIF.
renderSingleResult :: Int -> Types.SpeciesInformation -> Widget
renderSingleResult index information = do
  [whamlet| <div> Result <b># #{index}</b>|]
  mapM_ (\(t, g) -> showGenusField t (g information)) genusFields
  [whamlet|<br>|]
  mapM_ (\(Types.VernacularName t) -> [whamlet| #{t}<br>|])
    $ Types.speciesInformationVernacularNames information
  mapM_ (\t -> [whamlet| #{t}|])
    $ Types.speciesInformationStatuses information
  [whamlet| <br><hr><br>|]

  where
    genusFields :: [(String, Types.SpeciesInformation -> Maybe T.Text)]
    genusFields =
      [ ("Kingdom", Types.speciesInformationKingdom)
      , ("Phylum", Types.speciesInformationPhylum)
      , ("Order", Types.speciesInformationOrder)
      , ("Genus", Types.speciesInformationGenus)
      , ("Family", Types.speciesInformationFamily)
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
  initializeDatabase
  putStrLn $ spacer
          ++ "Serving application on port "
          ++ show port
          ++ "."
          ++ spacer
  warp port App
  where
    port = 3000
    spacer = take 4 $ repeat '\n'
