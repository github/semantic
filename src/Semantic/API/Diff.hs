{-# LANGUAGE GADTs, ConstraintKinds, TypeOperators, RankNTypes #-}
module Semantic.API.Diff
  ( parseDiffBuilder
  , doDiff
  , DiffEffects
  ) where

import Analysis.ConstructorName (ConstructorName)
import Analysis.TOCSummary (HasDeclaration)
import Control.Effect
import Control.Effect.Error
import Control.Exception
import Control.Monad.IO.Class
import Data.Blob
import Data.Diff
import Data.Language
import Data.Location
import Data.Term
import Diffing.Algorithm (Diffable)
import Parsing.Parser
import Prologue
import Semantic.Task as Task
import Semantic.Telemetry as Stat

import Data.ByteString.Builder
import           Rendering.Graph
import Serializing.Format hiding (JSON)
import qualified Serializing.Format as Format
import Rendering.JSON hiding (JSON)
import qualified Rendering.JSON
import Data.JSON.Fields

data DiffOutputFormat
  = DiffJSONTree
  | DiffJSONGraph
  | DiffSExpression
  deriving (Eq, Show)

parseDiffBuilder :: (Traversable t, DiffEffects sig m) => DiffOutputFormat -> t BlobPair -> m Builder
parseDiffBuilder DiffJSONTree  = distributeFoldMap (jsonDiff renderJSONTree) >=> serialize Format.JSON
parseDiffBuilder DiffJSONGraph = distributeFoldMap (jsonDiff renderJSONGraph) >=> serialize Format.JSON
parseDiffBuilder DiffSExpression = distributeFoldMap sexpDiff

type RenderJSON m syntax = forall syntax . CanDiff syntax => BlobPair -> Diff syntax Location Location -> m (Rendering.JSON.JSON "diffs" SomeJSON)

jsonDiff :: (DiffEffects sig m) => RenderJSON m syntax -> BlobPair -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonDiff f blobPair = doDiff blobPair (const pure) f `catchError` jsonError blobPair

jsonError :: Applicative m => BlobPair -> SomeException -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonError blobPair (SomeException e) = pure $ renderJSONDiffError blobPair (show e)

renderJSONTree :: (Applicative m, ToJSONFields1 syntax) => BlobPair -> Diff syntax Location Location -> m (Rendering.JSON.JSON "diffs" SomeJSON)
renderJSONTree blobPair = pure . renderJSONDiff blobPair

renderJSONGraph :: (Applicative m, Functor syntax, Foldable syntax, ConstructorName syntax) => BlobPair -> Diff syntax Location Location -> m (Rendering.JSON.JSON "diffs" SomeJSON)
renderJSONGraph blobPair = pure . renderJSONAdjDiff blobPair . renderTreeGraph

sexpDiff :: (DiffEffects sig m) => BlobPair -> m Builder
sexpDiff blobPair = doDiff blobPair (const pure) (const (serialize (SExpression ByConstructorName)))

type DiffEffects sig m = (Member (Error SomeException) sig, Member Telemetry sig, Member Distribute sig, Member Task sig, Carrier sig m, MonadIO m)

type CanDiff syntax = (ConstructorName syntax, Diffable syntax, Eq1 syntax, HasDeclaration syntax, Hashable1 syntax, Show1 syntax, ToJSONFields1 syntax, Traversable syntax)
type Decorate m a b = forall syntax . CanDiff syntax => Blob -> Term syntax a -> m (Term syntax b)

type TermPairConstraints =
 '[ ConstructorName
  , Diffable
  , Eq1
  , HasDeclaration
  , Hashable1
  , Show1
  , Traversable
  , ToJSONFields1
  ]

doDiff :: (DiffEffects sig m)
  => BlobPair -> Decorate m Location ann -> (forall syntax . CanDiff syntax => BlobPair -> Diff syntax ann ann -> m output) -> m output
doDiff blobPair decorate render = do
  SomeTermPair terms <- doParse blobPair decorate
  diff <- diffTerms blobPair terms
  render blobPair diff

diffTerms :: (CanDiff syntax, Member Task sig, Member Telemetry sig, Carrier sig m, MonadIO m)
  => BlobPair -> Join These (Term syntax ann) -> m (Diff syntax ann ann)
diffTerms blobs terms = time "diff" languageTag $ do
  diff <- diff (runJoin terms)
  diff <$ writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
  where languageTag = languageTagForBlobPair blobs

doParse :: (Member (Error SomeException) sig, Member Distribute sig, Member Task sig, Carrier sig m, Monad m)
  => BlobPair -> Decorate m Location ann -> m (SomeTermPair TermPairConstraints ann)
doParse blobPair decorate = case languageForBlobPair blobPair of
  Go         -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse goParser blob >>= decorate blob)
  Haskell    -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse haskellParser blob >>= decorate blob)
  Java       -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse javaParser blob >>= decorate blob)
  JavaScript -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse typescriptParser blob >>= decorate blob)
  JSON       -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse jsonParser blob >>= decorate blob)
  JSX        -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse typescriptParser blob >>= decorate blob)
  Markdown   -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse markdownParser blob >>= decorate blob)
  Python     -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse pythonParser blob >>= decorate blob)
  Ruby       -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse rubyParser blob >>= decorate blob)
  TypeScript -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse typescriptParser blob >>= decorate blob)
  PHP        -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse phpParser blob >>= decorate blob)
  _          -> noLanguageForBlob (pathForBlobPair blobPair)

data SomeTermPair typeclasses ann where
  SomeTermPair :: ApplyAll typeclasses syntax => Join These (Term syntax ann) -> SomeTermPair typeclasses ann

withSomeTermPair :: (forall syntax . ApplyAll typeclasses syntax => Join These (Term syntax ann) -> a) -> SomeTermPair typeclasses ann -> a
withSomeTermPair with (SomeTermPair terms) = with terms
