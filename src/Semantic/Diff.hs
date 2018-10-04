{-# LANGUAGE ConstraintKinds, GADTs, RankNTypes, ScopedTypeVariables #-}
module Semantic.Diff
  ( runDiff
  , diffBlobTOCPairs
  ) where

import Analysis.ConstructorName (ConstructorName)
import Analysis.Declaration (HasDeclaration, declarationAlgebra)
import Data.Blob
import Data.Diff
import Data.JSON.Fields
import Data.Location
import Data.Term
import Data.Graph.DiffVertex
import Diffing.Algorithm (Diffable)
import Parsing.Parser
import Prologue hiding (MonadError(..))
import Rendering.Graph
import Rendering.Renderer
import Semantic.IO (noLanguageForBlob)
import Semantic.Telemetry as Stat
import Semantic.Task as Task
import Serializing.Format
import Rendering.JSON (SomeJSON (..))
import qualified Rendering.JSON as JSON

-- | Using the specified renderer, diff a list of 'BlobPair's to produce a 'Builder' output.
runDiff :: (Member Distribute effs, Member (Exc SomeException) effs, Member (Lift IO) effs, Member Task effs, Member Telemetry effs) => DiffRenderer output -> [BlobPair] -> Eff effs Builder
runDiff ToCDiffRenderer         = withParsedBlobPairs (decorate . declarationAlgebra) (render . renderToCDiff) >=> serialize JSON
runDiff JSONDiffRenderer        = withParsedBlobPairs (const pure) (render . renderJSONDiff) >=> serialize JSON
runDiff JSONGraphDiffRenderer   = withParsedBlobPairs (const pure) (render . renderAdjGraph) >=> serialize JSON
  where renderAdjGraph :: (Recursive t, ToTreeGraph DiffVertex (Base t)) => BlobPair -> t -> JSON.JSON "diffs" SomeJSON
        renderAdjGraph blob diff = renderJSONAdjDiff blob (renderTreeGraph diff)
runDiff SExpressionDiffRenderer = withParsedBlobPairs (const pure) (const (serialize (SExpression ByConstructorName)))
runDiff ShowDiffRenderer        = withParsedBlobPairs (const pure) (const (serialize Show))
runDiff DOTDiffRenderer         = withParsedBlobPairs (const pure) (const (render renderTreeGraph)) >=> serialize (DOT (diffStyle "diffs"))

data SomeTermPair typeclasses ann where
  SomeTermPair :: ApplyAll typeclasses syntax => Join These (Term syntax ann) -> SomeTermPair typeclasses ann

withSomeTermPair :: (forall syntax . ApplyAll typeclasses syntax => Join These (Term syntax ann) -> a) -> SomeTermPair typeclasses ann -> a
withSomeTermPair with (SomeTermPair terms) = with terms

diffBlobTOCPairs :: (Member Distribute effs, Member (Exc SomeException) effs, Member (Lift IO) effs, Member Task effs, Member Telemetry effs) => [BlobPair] -> Eff effs ([TOCSummary], [TOCSummary])
diffBlobTOCPairs = withParsedBlobPairs (decorate . declarationAlgebra) (render . renderRPCToCDiff)

type CanDiff syntax = (ConstructorName syntax, Diffable syntax, Eq1 syntax, HasDeclaration syntax, Hashable1 syntax, Show1 syntax, ToJSONFields1 syntax, Traversable syntax)
type Decorate effs a b = forall syntax . CanDiff syntax => Blob -> Term syntax a -> Eff effs (Term syntax b)

withParsedBlobPairs :: (Member Distribute effs, Member (Exc SomeException) effs, Member (Lift IO) effs, Member Task effs, Member Telemetry effs, Monoid output)
                    => Decorate effs Location ann
                    -> (forall syntax . CanDiff syntax => BlobPair -> Diff syntax ann ann -> Eff effs output)
                    -> [BlobPair]
                    -> Eff effs output
withParsedBlobPairs decorate render = distributeFoldMap (\ blobs -> withParsedBlobPair decorate blobs >>= withSomeTermPair (diffTerms blobs >=> render blobs))
  where diffTerms :: (Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax, Member (Lift IO) effs, Member Task effs, Member Telemetry effs) => BlobPair -> Join These (Term syntax ann) -> Eff effs (Diff syntax ann ann)
        diffTerms blobs terms = time "diff" languageTag $ do
          diff <- diff (runJoin terms)
          diff <$ writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
          where languageTag = languageTagForBlobPair blobs

withParsedBlobPair :: (Member Distribute effs, Member (Exc SomeException) effs, Member Task effs)
                   => Decorate effs Location ann
                   -> BlobPair
                   -> Eff effs (SomeTermPair '[ConstructorName, Diffable, Eq1, HasDeclaration, Hashable1, Show1, ToJSONFields1, Traversable] ann)
withParsedBlobPair decorate blobs
  | Just (SomeParser parser) <- someParser @'[ConstructorName, Diffable, Eq1, HasDeclaration, Hashable1, Show1, ToJSONFields1, Traversable] (languageForBlobPair blobs)
    = SomeTermPair <$> distributeFor blobs (\ blob -> parse parser blob >>= decorate blob)
  | otherwise = noLanguageForBlob (pathForBlobPair blobs)
