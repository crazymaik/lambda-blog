module Lambda.View.SiteView
  ( renderSite
  ) where

import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.StringTemplate

import Lambda.Types
import Lambda.Model.Homepage
import Lambda.Model.Site
import Lambda.View.Common

renderSite :: Site -> HomepageHandler String
renderSite site =
  do t <- getST "site"
     hp <- ask
     let f = setAttribute "content" (snd $ fromJust $ siteContent site)
               . setAttribute "pageTitle" (siteMenuTitle site)
    
     renderST' $ f t

