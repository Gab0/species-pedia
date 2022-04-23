{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}

module Frontend where

import qualified Data.Text as T
import           Data.List

import           Yesod
import           Yesod.Form
import           Yesod.Core.Json
import           Yesod.Static

import           RemoteResources.GBIF
import           RemoteResources.Management

import           Foundation

import qualified Types

-- This module is DEPRECATED since we now use the dedicated React frontend.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- TODO: Implement favicon;
getFaviconR :: HandlerFor App Html
getFaviconR = defaultLayout horizontalLine

-- | Loads the React library remotely
-- (equals to the tag <script src="...">).
loadReact :: WidgetFor App ()
loadReact = do
  -- Load remote react.js libraries.
  addScriptRemote "https://unpkg.com/react@17/umd/react.development.js"
  addScriptRemote "https://unpkg.com/react-dom@17/umd/react-dom.development.js"
  -- Load the babel library remotely so we can use plain JSX scripts.
  addScriptRemote "https://unpkg.com/@babel/standalone/babel.min.js"

-- | Loads local scripts from the static directory.
loadLocalReact :: WidgetFor App ()
loadLocalReact = do
  scriptRouteBabel ["js", "widget.js"]
  scriptRouteBabel ["js", "page.js"]
  where
    -- Idiomatic way of adding .js scripts.
    scriptRoute r  = addScript
                   $ StaticR
                   $ StaticRoute r []
    -- Alternative way of adding .js scripts.
    -- We'll need to use this one because the `type` property is required.
    scriptRouteBabel r = [whamlet|<script type="text/babel" src="#{url}">|]
      where
        url = intercalate "/" $ "static": r
-- Endpoint for
getHomeReactR :: HandlerFor App Html
getHomeReactR =
  defaultLayout $ do
    loadReact
    loadLocalReact
    addStylesheet $ StaticR
                  $ StaticRoute ["css", "style.css"] []
    [whamlet|<div id="main">|]

postSearchReactR :: HandlerFor App Html
postSearchReactR =
  defaultLayout $ do
    [whamlet|PA|]

globalHeader :: WidgetFor App ()
globalHeader = do
    addStylesheet $ StaticR
                  $ StaticRoute ["css", "style.css"] []
    loadReact
    loadLocalReact
    --toWidgetHead [hamlet|<script src=@{StaticR js/widget.js}>|]
    [whamlet|
         <div id="title"><center><h1>Species Information Retriever</h1></center></div>
    |]
    toWidgetHead
        [hamlet|
            <meta name=keywords content="species information">
        |]


renderSearchForm :: Widget -> Enctype -> WidgetFor App ()
renderSearchForm form_widget enctype =
  [whamlet|<div class="search_form"><center><form method=post action=@{SearchR} enctype=#{enctype}>
              ^{form_widget}
            <button class="search-btn">Search</button>
            <div id="like">
    |]

-- Declare the query form.
-- This is better explained here:
-- https://www.yesodweb.com/book/forms#forms_kinds_of_forms
searchForm :: Html -> MForm Handler (FormResult Types.SpeciesQuery, Widget)
searchForm =  renderDivs
           $  Types.SpeciesQuery
          <$> areq textField "Search Query " Nothing
          <*> areq hiddenField "" (Just False)

-- Render the page that shows query results
postSearchR :: HandlerFor App Html
postSearchR = do
  ((result, widget), enctype) <- runFormPost searchForm

  defaultLayout $ do
    setTitle "Search Result"
    globalHeader
    renderSearchForm widget enctype
    case result of
      FormSuccess f -> do
        let query_string = Types.queryContent f

        either_content <- liftIO
                        $ manageCachedRemoteContent query_string
        showResultPage either_content

      _             -> [whamlet| Error|]

-- | Define encyclopedia search query JSON endpoint.
postSearchJ :: HandlerFor App Value
postSearchJ = do
  q <- requireCheckJsonBody :: Handler Types.SpeciesQuery
  let query_string = Types.queryContent q

  liftIO $ putStrLn $ "Encyclopedia search query: " <> T.unpack query_string
  either_content <- liftIO
                  $ manageCachedRemoteContent query_string

  returnJson either_content

showResultPage :: Either String Types.RemoteResult -> WidgetFor App ()
showResultPage (Right content) =
    [whamlet|<center>^{page_content}</center>|]
  where
    page_content = do
      [whamlet|<div>You searched for '#{query_string}'.|]
      [whamlet|<br><br>|]
      showResults content

    query_string = T.unpack
                 $ Types.remoteResultOriginalQuery content

    showResults (Types.RemoteResult _ _ results image_urls wikipedia _) = do
      case image_urls of
        Types.Retrieved imgs -> mapM_ (\image_url -> [whamlet|<img src="#{image_url}">|]) imgs
        _                    -> return ()
      horizontalLine
      showParagraph wikipedia
      horizontalLine
      renderSingleGBIFResult 1 results
showResultPage _               = [whamlet|Error processing request.|]



showParagraph :: Types.RemoteContent T.Text -> WidgetFor App ()
showParagraph (Types.Retrieved content) = [whamlet| #{content}|]
showParagraph _                         = [whamlet| Content not found.|]

horizontalLine = [whamlet|<hr>|]



-- Render information contained in a single SpeciesInformation.
renderSingleGBIFResult :: Int -> Types.SpeciesInformation -> Widget
renderSingleGBIFResult index information = do
  --[whamlet| <div> Result <b># #{index}</b>|]
  showCategory "Taxonomy"
  mapM_ (\(t, g) -> showGenusField t (g information)) genusFields

  [whamlet|<br>|]

  showCategory "Vernacular Names"
  mapM_ (\(Types.VernacularName t) -> showElement t)
    $ Types.speciesInformationVernacularNames information

  showCategory "Conservation Status"
  mapM_ showElement
    $ Types.speciesInformationStatuses information

  [whamlet|<br><hr><br>|]
  where
    showElement   :: T.Text -> Widget
    showElement t  = [whamlet|#{t}<br>|]
    showCategory  :: T.Text -> Widget
    showCategory t = [whamlet|<div class="information-category">#{t}|]

showGenusField :: String -> Maybe T.Text -> Widget
showGenusField name value =
  [whamlet|<div class='#{name}'>#{name}: #{n}</div>|]
  where
    n = case value of
      Just v  -> v
      Nothing -> "-"

-- This function renders the main page, which is also the search page.
-- 'julius' templates are for javascript code.
-- 'lucius' templates are for CSS style code.
-- 'hamlet' templates are for HTML code.
-- 'whamlet' temaplates are the same as 'hamlet',
--   but they can show widgets with #{}
--   and they are Widgets themselves
getHomeR :: HandlerFor App Html
getHomeR = do
    (widget, enctype) <- generateFormPost searchForm
    defaultLayout $ do
      setTitle "Species Searcher."

      globalHeader
      renderSearchForm widget enctype
      [whamlet|<center>This application searches for information about species in the internet.<br>
               Query for a species name such as <i>Felis catus</i>.|]
