module Lambda.App
  ( main
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.OldException (Exception)
import Data.Char
import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust, isJust)
import Network.FastCGI
import System.Directory
import System.Environment (getEnvironment)
import System.FilePath ((</>), takeFileName, takeExtension)
import Text.Pandoc
import Text.StringTemplate

import Lambda.Types
import Lambda.Feeds
import Lambda.Watchdog
import Lambda.Model.Author
import Lambda.Model.BlogPost
import Lambda.Model.Homepage
import Lambda.Model.Site
import Lambda.Model.Tag
import Lambda.View.BlogView
import Lambda.View.FourOhFour
import Lambda.View.SiteView

import Lambda.Util.Config
import Lambda.Util.IO
import qualified Lambda.Util.Log as L

main :: IO ()
main = 
  do dataDir <- getDataDir
     L.setupLogging "DEBUG" (dataDir </> "lambda.log")
     hp <- configLambdaBlog
     m <- newMVar hp
     L.info "Starting watchdog."
     let conf = hpConfig hp
         hosts = words $ conf M.! "watchdog.hosts"
         port = read $ conf M.! "watchdog.port"
     startWatchdog hosts port (loadAction m)
     L.info "Handling requests."
     serve m
     where
       loadAction m = do hp <- configLambdaBlog
                         swapMVar m hp
                         return ()

getDataDir = liftM (fromMaybe "./" . lookup "LAMBDA_BLOG_DIR") getEnvironment

configLambdaBlog :: IO Homepage
configLambdaBlog =
  do dataDir <- getDataDir
     config <- loadConfig $ dataDir </> "lambda.config"
     L.setLogLevel (M.findWithDefault "INFO" "logger.priority" config)
     L.info $ "Data directory is " ++ dataDir
     let authors = authorsFromMap config
     templates <- loadTemplates $ dataDir </> "templates"
     posts <- loadBlogPosts (dataDir </> "blog") authors
     let tags = tagsFromBlogPosts posts
         postsPerPage = read $ config M.! "blog.postsPerPage"
     sites <- loadSites (dataDir </> "site")
     return $ Homepage dataDir templates posts postsPerPage sites authors tags config

loadTemplates :: FilePath -> IO (STGroup String)
loadTemplates dir =
  do templateFiles <- findFilesInDirectory dir (\n -> ".st" == takeExtension n)
     liftM groupStringTemplates $ mapM loadTemplate templateFiles
     where
       loadTemplate file =
         do L.info $ "Loading template " ++ file
            content <- readFile file
            return (takeFileName $ take (length file - 3) file, newSTMP content)

serve :: MVar Homepage -> IO ()
serve = runFastCGIConcurrent' forkIO 10 . handle

handle :: MVar Homepage -> CGI CGIResult
handle m =
  do hp <- liftIO $ readMVar m
     url <- getVarWithDefault "REQUEST_URI" "/"
     lift $ L.info ("serving url " ++ url)
     catchCGI (runReaderT (handleUrl url) hp) handleError

handleUrl :: String -> HomepageHandler CGIResult
handleUrl url
  | url == "/" = handleBlog url "" "/p" (Just 1) id
  | "/t/" `isPrefixOf` url = handleBlog url ("Tag: " ++ url') ("/t/"++url') (number $ drop 1 us) (filter (\p -> url' `elem` postTags p))
  | "/a/" `isPrefixOf` url = handleBlog url ("Author: " ++ url') ("/a/"++url') (number $ drop 1 us) (filter (\p -> url' `elem` map authorId (postAuthors p)))
  | "/p/" `isPrefixOf` url = handleBlog url "" "/p" (number url') id
  | "/b/" `isPrefixOf` url =
    do hp <- ask
       let post = find (\p -> postId p == url') (hpBlogPosts hp)
       if isJust post then renderBlogFull (fromJust post) >>= output'
                      else fourohfour url
  | "/s/" `isPrefixOf` url =
    do hp <- ask
       let site = findSite (drop 3 url) (hpSites hp)
       if isJust site then renderSite (fromJust site) >>= output'
                      else fourohfour url
  | "/feed/atom.xml" == url = 
    do --lift $ setHeader "Content-type" "application/atom+xml"
       hp <- ask
       let title = hpConfig hp M.! "site.feed.title"
           baseUrl = hpConfig hp M.! "site.url"
           s = atomToString $ atomFeedFromEntries baseUrl title (take 15 $ hpBlogPosts hp)
       output' s
  | otherwise = fourohfour url
  where (url',us) = break (=='/') (drop 3 url)
        number :: String -> Maybe Integer
        number [] = Just 1
        number s = if all isNumber s
                     then Just $ read s
                     else Nothing
        findSite id [] = Nothing
        findSite id (x:xs) = if isJust found then found
                                             else findSite id xs
          where found = findSite' x
                findSite' s = if siteId s == id && isJust (siteContent s)
                                then Just s
                                else findSite id (siteSubSites s)

handleBlog :: String -> String -> String -> Maybe Integer -> ([BlogPost] -> [BlogPost]) -> HomepageHandler CGIResult
handleBlog url _ _ Nothing _ = fourohfour url
handleBlog url title url' (Just page) f =
  do hp <- ask
     let posts = f $ hpBlogPosts hp
         perPage = hpBlogPostsPerPage hp
         numPages = toInteger $ (length posts + perPage - 1) `div` perPage
         posts' = take perPage $ drop ((c page-1) * perPage) posts
         c = toEnum . fromIntegral
     if page > numPages then fourohfour url
                        else renderBlog url' title posts' (c page) (c numPages) >>= output'

output' = lift . output
setStatus' a b = lift $ setStatus a b

fourohfour :: String -> HomepageHandler CGIResult
fourohfour url = setStatus' 404 "Not Found" >> render404 url >>= output'

handleError :: Exception -> CGI CGIResult
handleError ex
  = do lift $ L.error $ "Exception: " ++ show ex
       setStatus 500 "Internal Error"
       setHeader "Content-type" "text/plain"
       output "Internal Error, Lambda-Blog is currently down"

