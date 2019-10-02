{-# LANGUAGE ConstraintKinds, ExistentialQuantification, GADTs, RankNTypes #-}
module Control.Effect.Parse
( -- * Parse effect
  Parse(..)
, parse
, parseWith
, parsePairWith
) where

import Control.Effect.Carrier
import Control.Effect.Error
import Control.Exception (SomeException)
import Data.Bifunctor.Join
import Data.Blob
import Data.Language
import Data.These
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


parseWith
  :: (Carrier sig m, Member (Error SomeException) sig, Member Parse sig)
  => [(Language, SomeParser c ann)]
  -> (forall term . c term => term ann -> m a)
  -> Blob
  -> m a
parseWith parsers with blob = case lookup (blobLanguage blob) parsers of
  Just (SomeParser parser) -> parse parser blob >>= with
  _                        -> noLanguageForBlob (blobPath blob)

parsePairWith
  :: (Carrier sig m, Member (Error SomeException) sig, Member Parse sig)
  => [(Language, SomeParser c ann)]
  -> (forall term . c term => Join These (term ann) -> m a)
  -> BlobPair
  -> m a
parsePairWith parsers with blobPair = case lookup (languageForBlobPair blobPair) parsers of
  Just (SomeParser parser) -> traverse (parse parser) blobPair >>= with
  _                        -> noLanguageForBlob (pathForBlobPair blobPair)
