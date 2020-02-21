module Semantic.Api.ScopeGraph (parseScopeGraph) where

import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Exception
import           Control.Lens
import           Data.Abstract.Declarations
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Foldable
import           Data.Functor.Foldable
import           Data.Language
import           Data.Map.Strict (Map)
import           Data.ProtoLens (defMessage)
import           Data.Term (IsTerm (..), TermF)
import           Data.Text (Text)
import           Data.Text (pack)
import qualified Parsing.Parser as Parser
import           Proto.Semantic as P hiding (Blob, BlobPair)
import           Proto.Semantic_Fields as P
import           Proto.Semantic_JSON ()
import           Semantic.Api.Bridge
import qualified Semantic.Api.LegacyTypes as Legacy
import           Semantic.Config
import           Semantic.Task
import           Serializing.Format (Format)
import           Source.Loc as Loc

parseScopeGraph :: ( Has Distribute sig m
                   , Has (Error SomeException) sig m
                   , Has (Reader PerLanguageModes) sig m
                   , Has Parse sig m, Traversable t
                   )
  => t Blob
  -> m ParseTreeScopeGraphResponse
parseScopeGraph blobs = pure $ defMessage & P.files .~ []
