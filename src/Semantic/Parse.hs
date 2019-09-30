{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}
module Semantic.Parse
( -- * Parse effect
  Parse(..)
, parse
  -- * Parse carrier
) where

import Control.Effect.Carrier
import Control.Monad.IO.Class
import Data.Blob
import Parsing.Parser

data Parse m k
  = forall term . Parse (Parser term) Blob (term -> m k)

deriving instance Functor m => Functor (Parse m)

instance HFunctor Parse where
  hmap f (Parse parser blob k) = Parse parser blob (f . k)

instance Effect Parse where
  handle state handler (Parse parser blob k) = Parse parser blob (handler . (<$ state) . k)


-- | Parse a 'Blob' with the given 'Parser'.
parse :: (Member Parse sig, Carrier sig m)
      => Parser term
      -> Blob
      -> m term
parse parser blob = send (Parse parser blob pure)


newtype ParseC m a = ParseC { runParse :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)
