module Lambda.Model.Site
  ( Site(..)
  , loadSites
  )
  where

import qualified Control.Exception as C
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import System.Directory
import System.FilePath
import System.Locale
import Text.Pandoc

import Lambda.Util.Config
import Lambda.Util.IO
import qualified Lambda.Util.Log as Log
import Lambda.Util.Prelude

data Site
  = Site
  { siteId :: String
  , siteMenuTitle :: String
  , siteContent :: Maybe (Pandoc, String)
  , siteSubSites :: [Site]
  } deriving (Show)

loadSites :: FilePath -> IO [Site]
loadSites dir = loadSites' dir ""

loadSites' :: FilePath -> FilePath -> IO [Site]
loadSites' dir idPath =
  do exist <- doesFileExist (dir </> "index.menu")
     if exist then do menu <- parseMenuFile (dir </> "index.menu")
                      m <- mapM (loadSite' idPath . (\(f,t) -> (dir </> f,f,t))) menu
                      return (catMaybes m)
              else return []
  where loadSite' f e = (loadSite f e >>= (return . Just)) `catch` (\e -> Log.error ("Error loading site " ++ idPath) >> return Nothing)

loadSite :: FilePath -> (FilePath, FilePath, String) -> IO Site
loadSite idPath (path,filename,title) =
  do Log.info $ "Loading site " ++ path
     if ".site" `isSuffixOf` path
       then do content <- readFile path
               let -- currently no map needed in site
                   --(meta, content') = break ("" ==) $ lines content
                   --(Right metaMap) = parseConfig path $ unlines meta
                   mdContent = readMarkdown defaultParserState content
               return $ Site
                 { siteId = idPath </> (dropExtension $ takeFileName path)
                 , siteMenuTitle = title
                 , siteContent = Just (mdContent, writeHtmlString defaultWriterOptions mdContent)
                 , siteSubSites = []
                 }
       else do exist <- doesFileExist $ path </> "index.site"
               subSites <- loadSites' path (idPath </> takeFileName path)
               if exist then do content <- readFile $ path </> "index.site"
                                let --(meta, content') = break ("" ==) $ lines content
                                    --(Right metaMap) = parseConfig path $ unlines meta
                                    mdContent = readMarkdown defaultParserState content
                                return $ Site
                                  { siteId = idPath </> takeFileName path
                                  , siteMenuTitle = title
                                  , siteContent = Just (mdContent, writeHtmlString defaultWriterOptions mdContent)
                                  , siteSubSites = subSites
                                  }
                        else do return $ Site
                                  { siteId = idPath </> takeFileName path
                                  , siteMenuTitle = title
                                  , siteContent = Nothing
                                  , siteSubSites = subSites
                                  }

parseMenuFile :: FilePath -> IO [(String, String)]
parseMenuFile file =
  do content <- readFile file
     return $ map (\(a,(b:bs)) -> (trim a, trim bs)) $ map (break (==':')) $ filter ((""/=) . trim) $ lines content

