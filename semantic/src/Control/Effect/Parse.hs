{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Control.Effect.Parse
( -- * Parse effect
  Parse(..)
, parse
, parserForLanguage
, parserForBlob
, parseWith
, parsePairWith
  -- * Re-exports
, Algebra
, Has
, run
) where

import           Control.Algebra
import           Control.Effect.Error
import           Control.Exception (SomeException)
import           Data.Bitraversable
import           Data.Blob
import           Data.Edit
import           Data.Kind (Type)
import qualified Data.Map as Map
import           Parsing.Parser
import           Source.Language (Language (..))

data Parse (m :: Type -> Type) k where
  Parse :: Parser term -> Blob -> Parse m term


-- | Parse a 'Blob' with the given 'Parser'.
parse :: Has Parse sig m
      => Parser term
      -> Blob
      -> m term
parse parser blob = send (Parse parser blob)


-- | Select a parser for the given 'Language'.
parserForLanguage :: Map.Map Language (SomeParser c ann) -> Language -> Maybe (SomeParser c ann)
parserForLanguage = flip Map.lookup

-- | Select a parser for the given 'Blob'.
parserForBlob :: Map.Map Language (SomeParser c ann) -> Blob -> Maybe (SomeParser c ann)
parserForBlob parsers = parserForLanguage parsers . blobLanguage


-- | Parse a 'Blob' with one of the provided parsers, and run an action on the abstracted term.
parseWith
  :: (Has (Error SomeException) sig m, Has Parse sig m)
  => Map.Map Language (SomeParser c ann)       -- ^ The set of parsers to select from.
  -> (forall term . c term => term ann -> m a) -- ^ A function to run on the parsed term. Note that the term is abstract, but constrained by @c@, allowing you to do anything @c@ allows, and requiring that all the input parsers produce terms supporting @c@.
  -> Blob                                      -- ^ The blob to parse.
  -> m a
parseWith parsers with blob = case parserForBlob parsers blob of
  Just (SomeParser parser) -> parse parser blob >>= with
  _                        -> noLanguageForBlob (blobPath blob)

-- | Parse a 'BlobPair' with one of the provided parsers, and run an action on the abstracted term pair.
parsePairWith
  :: (Has (Error SomeException) sig m, Has Parse sig m)
  => Map.Map Language (SomeParser c ann)                                     -- ^ The set of parsers to select from.
  -> (forall term . c term => Edit (Blob, term ann) (Blob, term ann) -> m a) -- ^ A function to run on the parsed terms. Note that the terms are abstract, but constrained by @c@, allowing you to do anything @c@ allows, and requiring that all the input parsers produce terms supporting @c@.
  -> BlobPair                                                                -- ^ The blob pair to parse.
  -> m a
parsePairWith parsers with blobPair = case parserForLanguage parsers (languageForBlobPair blobPair) of
  Just (SomeParser parser) -> bitraverse (p parser) (p parser) blobPair >>= with
  _                        -> noLanguageForBlob (pathForBlobPair blobPair)
  where p parser blob = (,) blob <$> parse parser blob
