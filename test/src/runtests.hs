import Control.Applicative
import System
import System.Directory
import System.FilePath (combine)
import List

testfile = "AllTests.hs"

main :: IO ()
main = do
  files <- collectFiles "test"
  tests <- mapM processFile files
  writeFile testfile $ unlines $ header ++ ["import qualified " ++ name | (name,_) <- tests]
    ++ ["\nmain = do\n  putStrLn \"----- starting tests -----\""] ++ (concat [genPropTests t | t <- tests])
    ++ (concat [genUnitTests t | t <- tests]) ++ ["  putStrLn \"\\n----- tests finished -----\""]
  system("runghc -isrc:test/src -fglasgow-exts " ++ testfile)
  removeFile testfile

unlit [] = []
unlit x  = if (head x) == '>' then (tail x) else x

collectFiles path = do
  isFile <- doesFileExist path
  if isFile
    then do
      if "Tests.hs" `isSuffixOf` path
          then return [path]
          else return []
    else do
      isDir <- doesDirectoryExist path
      if isDir
        then do
          paths <- getDirectoryContents path
          let paths' = map (\x -> combine path x) $ filter (\x -> head x /= '.') paths
          concat <$> mapM collectFiles paths'
        else return []

processFile file = do
  s <- (filter (""/=)) . lines <$> readFile file
  let modName = takeWhile (' '/=) $ drop 7 $ head s
  let names search = nub $ filter (\x -> (search `isPrefixOf` x)) $ map (fst.head.lex.unlit) s
  let props = names "prop_"
  let units = names "unit_"
  return (modName,(props,units))

header = ["import Test.HUnit","import Test.QuickCheck","import Text.Printf"]

genPropTests (_,([],_)) = []
genPropTests (mod,(tests,_)) = map propTest tests
  where
    propTest t = "  printf \"%-25s: \" \"" ++ t ++ "\"\n  quickCheck " ++ mod ++ "." ++ t

genUnitTests (_,(_,[])) = []
genUnitTests (mod,(_,tests)) = printFileName : map unitTest tests
  where
    printFileName = ("  printf \"\\nFile: %s\\n\" \"" ++ mod ++ "\"\n")
    unitTest t = "  runTestTT $ TestLabel \"" ++ t ++ "\"" ++ mod ++ "." ++ t ++ "\n"

