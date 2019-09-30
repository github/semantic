{-# LANGUAGE ExistentialQuantification #-}
module Semantic.Parse
( Parse(..)
) where

import Data.Blob
import Parsing.Parser

data Parse m k
  = forall term . Parse (Parser term) Blob (term -> m k)

deriving instance Functor m => Functor (Parse m)
