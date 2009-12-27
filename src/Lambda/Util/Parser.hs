module Lambda.Util.Parser where

import Control.Applicative hiding ((<|>),many)
import Control.Monad (return,ap)
import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos

instance Applicative (GenParser c st) where
  pure = return
  (<*>) = ap

instance Error ParseError where
  noMsg    = newErrorUnknown (initialPos "")
  strMsg s = newErrorMessage (Message s) (initialPos "")

-- | parses end of line or end of file.
eol :: GenParser Char st ()
eol = (newline >> return ()) <|> eof <?> "end of line"

-- | parses a (maybe empty) line.
line' :: CharParser st String
line' = manyTill anyChar eol

