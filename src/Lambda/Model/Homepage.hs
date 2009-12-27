module Lambda.Model.Homepage
  ( Homepage(..)
  ) where

import Text.StringTemplate

import Lambda.Model.Author
import Lambda.Model.BlogPost
import Lambda.Model.Site
import Lambda.Model.Tag
import Lambda.Util.Config

data Homepage = Homepage
              { hpDirectory :: FilePath
              , hpTemplates :: STGroup String
              , hpBlogPosts :: [BlogPost]
              , hpBlogPostsPerPage :: Int
              , hpSites :: [Site]
              , hpAuthors :: [Author]
              , hpTags :: [Tag]
              , hpConfig :: ConfigMap
              }

