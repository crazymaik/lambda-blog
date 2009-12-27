module Lambda.Feeds
  ( atomToString
  , atomFeedFromEntries
  ) where

import Data.Time.Format
import System.Locale
import Text.Atom.Feed
import qualified Text.Feed.Types as F
import Text.Feed.Export
import Text.XHtml.Strict (showHtml)
import Text.XML.Light.Output

import Lambda.Util.Prelude
import Lambda.Model.Author
import Lambda.Model.BlogPost

atomToString :: Feed -> String
atomToString f = showTopElement $ xmlFeed $ F.AtomFeed f

atomFeedFromEntries :: String -> String -> [BlogPost] -> Feed
atomFeedFromEntries url title e =
  let date = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:00Z" (postDate $ head e)
      link = url ++ "/feed/atom.xml"
  in Feed {
    feedId = link,
    feedTitle = HTMLString title,
    feedUpdated = date,
    feedAuthors = [],
    feedCategories = [],
    feedContributors = [],
    feedGenerator = Nothing,
    feedIcon = Nothing,
    feedLinks = [Link link (Just $ Left "self") Nothing Nothing Nothing Nothing [] []],
    feedLogo = Nothing,
    feedRights = Nothing,
    feedSubtitle = Nothing,
    feedEntries = map (blogEntryToAtomEntry url title) e,
    feedAttrs = [],
    feedOther = []
  }

blogEntryToAtomEntry :: String -> String -> BlogPost -> Entry
blogEntryToAtomEntry url title e =
  let date = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:00Z" (postDate e)
      link = url ++ "/b/" ++ (postId e)
  in Entry {
    entryId = link++":"++date,
    entryTitle = TextString $ postTitle e,
    entryUpdated = date,
    entryAuthors = map (\a -> Person {
                      personName = authorName a,
                      personURI = Just (url ++ "/a/" ++ authorId a),
                      personEmail = Just $ authorMail a,
                      personOther = []
                    }) (postAuthors e),
    entryCategories = [],
    entryContent = Nothing, --Just $ HTMLContent $ showHtml $ B.entryContent e,
    entryContributor = [],
    entryLinks = [Link link Nothing Nothing Nothing Nothing Nothing [] []],
    entryPublished = Just date,
    entryRights = Nothing,
    entrySource = Nothing,
    entrySummary = Nothing,
    entryInReplyTo = Nothing,
    entryInReplyTotal = Nothing,
    entryAttrs = [],
    entryOther = []
  }

