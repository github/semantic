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
  => Map.Map Language (SomeParser c ann)       -- ^ The set of parsers to select from.
  -> (forall term . c term => term ann -> m a) -- ^ A function to run on the parsed term. Note that the term is abstract, but constrained by @c@, allowing you to do anything @c@ allows, and requiring that all the input parsers produce terms supporting @c@.
  -> Blob                                      -- ^ The blob to parse.
  -> m a
parseWith parsers with blob = case Map.lookup (blobLanguage blob) parsers of
  Just (SomeParser parser) -> parse parser blob >>= with
  _                        -> noLanguageForBlob (blobPath blob)

-- | Parse a 'BlobPair' with one of the provided parsers, and run an action on the abstracted term pair.
parsePairWith
  :: (Carrier sig m, Member (Error SomeException) sig, Member Parse sig)
  => Map.Map Language (SomeParser c ann)                          -- ^ The set of parsers to select from.
  -> (forall term . c term => These (term ann) (term ann) -> m a) -- ^ A function to run on the parsed terms. Note that the terms are abstract, but constrained by @c@, allowing you to do anything @c@ allows, and requiring that all the input parsers produce terms supporting @c@.
  -> BlobPair                                                     -- ^ The blob pair to parse.
  -> m a
parsePairWith parsers with blobPair = case Map.lookup (languageForBlobPair blobPair) parsers of
  Just (SomeParser parser) -> traverse (parse parser) blobPair >>= with . runJoin
  _                        -> noLanguageForBlob (pathForBlobPair blobPair)
