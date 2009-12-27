module Lambda.View.BlogView
  ( renderBlog
  , renderBlogFull
  ) where

import qualified Data.Map as M
import Control.Monad.Reader
import Text.StringTemplate

import Lambda.Types
import Lambda.Model.BlogPost
import Lambda.Model.Homepage

import Lambda.View.Common

renderBlog :: String -> String -> [BlogPost] -> Int -> Int -> HomepageHandler String
renderBlog url title posts idx ps =
  do t <- getST "blog"
     hp <- ask
     let numbers = iterate (+1) 1
         after = take (ps-idx) $ drop idx numbers
         before = take (idx-1) numbers
         f = setAttribute "posts" posts
                . setAttribute "baseUrl" url
                . setAttribute "idxbefore" (map show before)
                . setAttribute "idxcurrent" (show idx)
                . setAttribute "idxafter" (map show after)
                . setAttribute "pageTitle" title
     renderST' (f t)

renderBlogFull :: BlogPost -> HomepageHandler String
renderBlogFull post =
  do t <- getST "blogFull"
     hp <- ask
     let f = setAttribute "posts" [post]
               . setAttribute "pageTitle" (postTitle post)
     renderST' $ f t

