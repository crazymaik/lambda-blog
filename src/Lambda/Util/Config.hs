-- | Properties contain string key-value mappings
--
-- A mapping is separated by @=@ or @:@
-- @
--   key = value
-- @
--
-- A key may start with any letter or the underscore '@_@', followed by
-- any letter, digit or one of the following chars '@_.-@'.
--
-- A value is whitespace-trimmed.
module Lambda.Util.Config
  ( loadConfig
  , parseConfig
  , ConfigMap
  ) where

import Control.Applicative hiding ((<|>), many)
import Control.Monad (when, unless)
import Data.List
import Data.Maybe
import qualified Data.Map as M
import System.Directory (doesFileExist)
import Text.ParserCombinators.Parsec

import Lambda.Util.Parser
import Lambda.Util.Prelude

type ConfigMap = M.Map String String

loadConfig :: FilePath -> IO ConfigMap
loadConfig filename =
  do exists <- doesFileExist filename
     unless exists $ fail ("File '" ++ filename ++ "' does not exist.")
     content <- readFile filename
     case parseConfig filename content of
       (Left x) -> fail x
       (Right x) -> return x

parseConfig :: FilePath -> String -> Either String ConfigMap
parseConfig = parseProperties

parseProperties :: String -> String -> Either String ConfigMap
parseProperties sourceName content =
  case parse properties sourceName content of
    (Left x) -> (Left $ show x)
    (Right x) -> (Right x)

properties :: Parser ConfigMap
properties = (M.fromList . catMaybes) <$> many (try comment <|> mapping <* spaces) <* eol

comment :: Parser (Maybe (String,String))
comment = (const Nothing) <$> (spaces >> char '#' >> line')

mapping :: Parser (Maybe (String,String))
mapping = (curry Just) <$> (spaces >> key) <*> (spaces >> oneOf ":=" >> line' >>= (return . trim))

key :: Parser String
key =
  do ch <- letter <|> char '_'
     rest <- many (letter <|> digit <|> oneOf "_.-")
     return (ch:rest)

