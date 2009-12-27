module Lambda.Types
  ( HomepageHandler
  ) where

import Control.Monad.Reader
import Network.FastCGI

import Lambda.Model.Homepage

type HomepageHandler = ReaderT Homepage (CGIT IO)

