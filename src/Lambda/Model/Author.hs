module Lambda.Model.Author
  ( Author(..)
  , authorsFromMap
  ) where

import qualified Data.Map as M

data Author
  = Author
  { authorId :: String
  , authorName :: String
  , authorNick :: String
  , authorMail :: String
  , authorDelicious :: String
  } deriving (Show)

authorsFromMap :: M.Map String String -> [Author]
authorsFromMap m =
  let names = words $ m M.! "authors.ids"
  in map (flip authorFromMap m) names

authorFromMap :: String -> M.Map String String -> Author
authorFromMap authId m =
  let lookup k = m M.! ("authors." ++ authId ++ "." ++ k)
  in Author
       { authorId = authId
       , authorName = lookup "name"
       , authorNick = lookup "nick"
       , authorMail = lookup "mail"
       , authorDelicious = lookup "delicious"
       }

