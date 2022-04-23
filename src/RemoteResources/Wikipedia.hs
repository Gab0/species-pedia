module RemoteResources.Wikipedia where

import           Control.Monad (mfilter)
import qualified Data.Text as T
import           Data.Maybe

import           Text.HTML.TagSoup

import           RemoteResources.Core

import           Types

-- | Top level function to retriee wikipedia information.
retrieveWikipedia :: String -> IO (RemoteContent T.Text)
retrieveWikipedia species_name = do
  from_remote <- fetchParagraph species_name
  return $ case from_remote of
    Just res ->
      case (formatParagraph .  parseParagraph) res of
        Just r  -> Retrieved r
        Nothing -> NotAvailable
    Nothing  -> NotAvailable


replace :: Char -> Char -> String -> String
replace a b = map
            $ maybe b id . mfilter (/= a) . Just

formatParagraph :: [String] -> Maybe T.Text
formatParagraph [] = Nothing
formatParagraph t  = Just
                   $ T.pack
                   $ unlines t

fetchParagraph :: String -> IO (Maybe String)
fetchParagraph identifier =
   makeGetRequest $ buildWikipediaUrl identifier


buildWikipediaUrl :: String -> String
buildWikipediaUrl = (++) base_url . formatIdentifier
  where
    base_url         = "https://en.wikipedia.org/wiki/"
    formatIdentifier = replace ' ' '_'

parseParagraph :: String -> [String]
parseParagraph = mapMaybe locateParagraphs
               . (\wt -> extractText False wt [])
               . parseTags
  where
    locateParagraphs :: Tag String -> Maybe String
    locateParagraphs tag
      | isValidTag tag = Just
                       $ fromTagText tag
      | otherwise      = Nothing
        where
          isValidTag = isTagText

-- | 
identifyTag :: Tag String -> Maybe Bool
identifyTag (TagOpen "p" _) = Just True
identifyTag (TagClose "p")  = Just False
identifyTag _               = Nothing

-- | Extracts all tags that are between <p> and </p> HTML tags.
-- Recursive function.
extractText :: Bool -> [Tag String] -> [Tag String] -> [Tag String]
extractText _ [] c = c
extractText state remaining_tags current_tags =
  extractText next_state next_tags $
  case next_state of
    True  -> current_tags ++ [tag]
    False -> current_tags
  where
    next_state    = defineState state new_state
    new_state     = identifyTag tag
    tag:next_tags = remaining_tags
    defineState prev Nothing = prev
    defineState _   (Just s) = s
