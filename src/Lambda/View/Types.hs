module Lambda.View.Types where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Format
import System.Locale
import Text.StringTemplate
import Text.StringTemplate.Classes

import Lambda.Model.Author
import Lambda.Model.BlogPost
import Lambda.Model.Site
import Lambda.Model.Tag

instance ToSElem Author where
  toSElem a = SM $ M.fromList [ ("id", toSElem $ authorId a)
                              , ("name", toSElem $ authorName a)
                              , ("mail", toSElem $ authorMail a)
                              , ("nick", toSElem $ authorNick a)
                              , ("delicious", toSElem $ authorDelicious a)]

instance ToSElem BlogPost where
  toSElem p = SM $ M.fromList [ ("id", toSElem $ postId p)
                              , ("title", toSElem $ postTitle p)
                              , ("authors", toSElem $ postAuthors p)
                              , ("date", toSElem $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (postDate p))
                              , ("tags", LI $ map toSElem $ postTags p)
                              , ("content", toSElem $ postContentHtml p)]

instance ToSElem Site where
  toSElem s = SM $ M.fromList [ ("id", toSElem $ siteId s)
                              , ("title", toSElem $ siteMenuTitle s)
                              , ("hasContent", toSElem $ isJust $ siteContent s)
                              , ("subs", toSElem $ siteSubSites s)]

instance ToSElem Tag where
  toSElem t = SM $ M.fromList [ ("id", toSElem $ tagId t)
                              , ("count", toSElem $ show $ tagCount t)]

