module Lambda.View.FourOhFour
  ( render404
  ) where

import Lambda.Types
import Lambda.View.Common

render404 :: String -> HomepageHandler String
render404 url = renderST "404" "invalidUrl" url

