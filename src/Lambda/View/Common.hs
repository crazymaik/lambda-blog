module Lambda.View.Common
  ( getST
  , renderST
  , renderST'
  ) where

import Control.Monad.Reader
import Data.Maybe (fromJust)
import Text.StringTemplate

import Lambda.Types
import Lambda.Model.Homepage
import Lambda.View.Types

getST :: String -> HomepageHandler (StringTemplate String)
getST t = do hp <- ask
             let template = fromJust $ getStringTemplate t (hpTemplates hp)
             return template

renderST :: (ToSElem a) => String -> String -> a -> HomepageHandler String
renderST template key value =
  do t <- getST template
     hp <- ask
     let t' = setSidebar hp $ setAttribute key value t
     return $ render t'

renderST' :: StringTemplate String -> HomepageHandler String
renderST' template =
  do hp <- ask
     let t' = setSidebar hp template
     return $ render t'

setSidebar :: (Stringable b) => Homepage -> StringTemplate b -> StringTemplate b
setSidebar hp =
  (setAttribute "authors" $ hpAuthors hp)
    . (setAttribute "tags" $ hpTags hp)
    . (setAttribute "sites" $ hpSites hp)

