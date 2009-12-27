module Lambda.Model.Tag
  ( Tag(..)
  , tagsFromBlogPosts
  ) where

import Data.List

import Lambda.Model.BlogPost

data Tag
  = Tag
  { tagId :: String
  , tagCount :: Int
  } deriving (Show)

tagsFromBlogPosts :: [BlogPost] -> [Tag]
tagsFromBlogPosts posts = map (\t -> Tag (head t) (length t)) tags
  where tags = (group . sort . concat) $ map postTags posts

