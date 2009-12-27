module Lambda.Util.IO where

import Control.Monad (filterM, liftM)
import System.FilePath
import System.IO
import System.Directory

findFilesInDirectory :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
findFilesInDirectory dir f =
  do content <- getDirectoryContents dir
     let files = filter f content
     filterM doesFileExist $ map (dir </>) files

