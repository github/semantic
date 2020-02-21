module Semantic.Api.ScopeGraph (parseScopeGraph) where

import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Exception
import           Control.Lens
import           Data.Blob
import           Data.Language
import           Data.ProtoLens (defMessage)
import           Proto.Semantic as P hiding (Blob, BlobPair)
import           Proto.Semantic_Fields as P
import           Proto.Semantic_JSON ()
import           Semantic.Task

parseScopeGraph :: ( Has Distribute sig m
                   , Has (Error SomeException) sig m
                   , Has (Reader PerLanguageModes) sig m
                   , Has Parse sig m, Traversable t
                   )
  => t Blob
  -> m ParseTreeScopeGraphResponse
parseScopeGraph blobs = pure $ defMessage & P.files .~ []
