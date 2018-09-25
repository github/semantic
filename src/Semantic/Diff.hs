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
runDiff JSONDiffRenderer        = withParsedBlobPairs (decorate . unitAlgebra) (\blob -> render (renderJSONDiff blob) . bimap snd snd) >=> serialize JSON
runDiff JSONGraphDiffRenderer   = withParsedBlobPairs (decorate . unitAlgebra) (\blob -> render (renderAdjGraph blob) . bimap snd snd) >=> serialize JSON
  where renderAdjGraph :: (Recursive t, ToTreeGraph DiffVertex (Base t)) => BlobPair -> t -> JSON.JSON "diffs" SomeJSON
        renderAdjGraph blob diff = renderJSONAdjDiff blob (renderTreeGraph diff)
runDiff SExpressionDiffRenderer = withParsedBlobPairs (decorate . unitAlgebra) (const (serialize (SExpression ByConstructorName)))
runDiff ShowDiffRenderer        = withParsedBlobPairs (decorate . unitAlgebra) (const (serialize Show))
runDiff DOTDiffRenderer         = withParsedBlobPairs (decorate . unitAlgebra) (\_ -> render renderTreeGraph . bimap snd snd) >=> serialize (DOT (diffStyle "diffs"))

unitAlgebra :: Blob -> RAlgebra (TermF syntax Location) (Term syntax Location) (DiffAnnotation ())
unitAlgebra _ (In ann _) = ((), ann)

data SomeTermPair typeclasses ann where
  SomeTermPair :: ApplyAll typeclasses syntax => Join These (Term syntax ann) -> SomeTermPair typeclasses ann

withSomeTermPair :: (forall syntax . ApplyAll typeclasses syntax => Join These (Term syntax ann) -> a) -> SomeTermPair typeclasses ann -> a
withSomeTermPair with (SomeTermPair terms) = with terms

diffBlobTOCPairs :: (Member Distribute effs, Member (Exc SomeException) effs, Member (Lift IO) effs, Member Task effs, Member Telemetry effs) => [BlobPair] -> Eff effs ([TOCSummary], [TOCSummary])
diffBlobTOCPairs = withParsedBlobPairs (decorate . declarationAlgebra) (render . renderRPCToCDiff)

type CanDiff syntax = (ConstructorName syntax, Diffable syntax, Eq1 syntax, HasDeclaration syntax, Hashable1 syntax, Show1 syntax, ToJSONFields1 syntax, Traversable syntax)
type Decorate effs a b = forall syntax . CanDiff syntax => Blob -> Term syntax a -> Eff effs (Term syntax b)

withParsedBlobPairs :: (Member Distribute effs, Member (Exc SomeException) effs, Member (Lift IO) effs, Member Task effs, Member Telemetry effs, Monoid output)
                    => Decorate effs Location (DiffAnnotation a)
                    -> (forall syntax . CanDiff syntax => BlobPair -> Diff syntax (DiffAnnotation a) (DiffAnnotation a) -> Eff effs output)
                    -> [BlobPair]
                    -> Eff effs output
withParsedBlobPairs decorate render = distributeFoldMap (\ blobs -> withParsedBlobPair decorate blobs >>= withSomeTermPair (diffTerms blobs >=> render blobs))
  where diffTerms :: (Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax, Member (Lift IO) effs, Member Task effs, Member Telemetry effs) => BlobPair -> Join These (Term syntax (DiffAnnotation a)) -> Eff effs (Diff syntax (DiffAnnotation a) (DiffAnnotation a))
        diffTerms blobs terms = time "diff" languageTag $ do
          diff <- diff (runJoin terms)
          diff <$ writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
          where languageTag = languageTagForBlobPair blobs

withParsedBlobPair :: (Member Distribute effs, Member (Exc SomeException) effs, Member Task effs)
                   => Decorate effs Location (DiffAnnotation a)
                   -> BlobPair
                   -> Eff effs (SomeTermPair '[ConstructorName, Diffable, Eq1, HasDeclaration, Hashable1, Show1, ToJSONFields1, Traversable] (DiffAnnotation a))
withParsedBlobPair decorate blobs
  | Just (SomeParser parser) <- someParser @'[ConstructorName, Diffable, Eq1, HasDeclaration, Hashable1, Show1, ToJSONFields1, Traversable] (languageForBlobPair blobs)
    = SomeTermPair <$> distributeFor blobs (\ blob -> parse parser blob >>= decorate blob)
  | otherwise = noLanguageForBlob (pathForBlobPair blobs)
