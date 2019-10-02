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
import qualified Data.Map as Map
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


-- | Parse a 'Blob' with one of the provided parsers, and run an action on the abstracted term.
parseWith
  :: (Carrier sig m, Member (Error SomeException) sig, Member Parse sig)
  => Map.Map Language (SomeParser c ann)
  -> (forall term . c term => term ann -> m a)
  -> Blob
  -> m a
parseWith parsers with blob = case Map.lookup (blobLanguage blob) parsers of
  Just (SomeParser parser) -> parse parser blob >>= with
  _                        -> noLanguageForBlob (blobPath blob)

-- | Parse a 'BlobPair' with one of the provided parsers, and run an action on the abstracted term pair.
parsePairWith
  :: (Carrier sig m, Member (Error SomeException) sig, Member Parse sig)
  => Map.Map Language (SomeParser c ann)
  -> (forall term . c term => These (term ann) (term ann) -> m a)
  -> BlobPair
  -> m a
parsePairWith parsers with blobPair = case Map.lookup (languageForBlobPair blobPair) parsers of
  Just (SomeParser parser) -> traverse (parse parser) blobPair >>= with . runJoin
  _                        -> noLanguageForBlob (pathForBlobPair blobPair)
