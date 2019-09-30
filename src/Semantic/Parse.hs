{-# LANGUAGE ExistentialQuantification #-}
module Semantic.Parse
( Parse(..)
) where

import Control.Effect.Carrier
import Data.Blob
import Parsing.Parser

data Parse m k
  = forall term . Parse (Parser term) Blob (term -> m k)

deriving instance Functor m => Functor (Parse m)

instance HFunctor Parse where
  hmap f (Parse parser blob k) = Parse parser blob (f . k)
