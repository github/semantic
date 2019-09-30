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

instance Effect Parse where
  handle state handler (Parse parser blob k) = Parse parser blob (handler . (<$ state) . k)
