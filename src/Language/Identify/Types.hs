module Language.Identify.Types ( Profile(..)
                               , NGrams(..)  ) where

import qualified Data.Map as M
import qualified Data.Text as T

type NGrams = M.Map T.Text Int

data Profile = Profile { name :: T.Text
                       , ngrams :: NGrams }
               deriving (Show, Eq, Read)
               
