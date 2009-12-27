module Lambda.Model.BlogPost
  ( BlogPost(..)
  , loadBlogPosts
  )
  where

import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import System.FilePath
import System.Locale
import Text.Pandoc

import Lambda.Model.Author
import Lambda.Util.Config
import Lambda.Util.IO
import qualified Lambda.Util.Log as Log
import Lambda.Util.Prelude

data BlogPost
  = BlogPost {
    postId :: String,
    postTitle :: String,
    postDate :: UTCTime,
    postAuthors :: [Author],
    postTags :: [String],
    postContent :: Pandoc,
    postContentHtml :: String
  } deriving (Show)

loadBlogPosts :: FilePath -> [Author] -> IO [BlogPost]
loadBlogPosts dir authors = 
  do files <- findFilesInDirectory dir (\n -> ".post" == takeExtension n)
     let authorsMap = M.fromList $ map (\a -> (authorId a, a)) authors
     posts <- mapM (load authorsMap) files
     return $ reverse $ sortUsing postDate $ catMaybes posts
  where load m fp = (loadBlogPost fp m >>= return) `catch` (\e -> Log.error ("Loading blog post failed: " ++ fp) >> return Nothing)

loadBlogPost :: FilePath -> M.Map String Author -> IO (Maybe BlogPost)
loadBlogPost filename authors =
  do fileContent <- readFile filename
     let (meta, content) = break ("" ==) $ lines fileContent
         (Right metaMap) = parseConfig filename $ unlines meta
         mdContent = readMarkdown defaultParserState $ unlines content
         id' = dropExtension $ takeFileName filename
         opts = defaultWriterOptions { writerIdentifierPrefix = id' ++ "-" }
     if metaMap M.! "state" == ""
       then return Nothing
       else return $ Just $ BlogPost
            { postId = id'
            , postTitle = metaMap M.! "title"
            , postDate = readTime defaultTimeLocale "%Y-%m-%d %H:%M" (metaMap M.! "date")
            , postAuthors = map (authors M.!) $ words $ metaMap M.! "authors"
            , postTags = words $ metaMap M.! "tags"
            , postContent = mdContent
            , postContentHtml = writeHtmlString opts mdContent
            }

