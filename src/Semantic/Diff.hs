{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Semantic.Diff where

import Analysis.ConstructorName (ConstructorName, constructorLabel)
import Analysis.IdentifierName (IdentifierName, identifierLabel)
import Analysis.Declaration (HasDeclaration, declarationAlgebra)
import Data.AST
import Data.Blob
import Data.Diff
import Data.JSON.Fields
import Data.Record
import Data.Term
import Diffing.Algorithm (Diffable)
import Parsing.Parser
import Prologue hiding (MonadError(..))
import Rendering.Graph
import Rendering.Renderer
import Semantic.IO (noLanguageForBlob)
import Semantic.Stat as Stat
import Semantic.Task as Task
import Serializing.Format

runDiff :: Members '[Distribute WrappedTask, Task, Telemetry, Exc SomeException, IO] effs => DiffRenderer output -> [BlobPair] -> Eff effs Builder
runDiff ToCDiffRenderer         = withParsedBlobPairs (decorate . declarationAlgebra) (render . renderToCDiff) >=> serialize JSON
runDiff JSONDiffRenderer        = withParsedBlobPairs (const (decorate constructorLabel >=> decorate identifierLabel)) (render . renderJSONDiff) >=> serialize JSON
runDiff SExpressionDiffRenderer = withParsedBlobPairs (const pure) (const (serialize (SExpression ByConstructorName)))
runDiff DOTDiffRenderer         = withParsedBlobPairs (const pure) (const (render renderTreeGraph)) >=> serialize (DOT (diffStyle "diffs"))

data SomeTermPair typeclasses ann where
  SomeTermPair :: ApplyAll typeclasses syntax => Join These (Term syntax ann) -> SomeTermPair typeclasses ann

withSomeTermPair :: (forall syntax . ApplyAll typeclasses syntax => Join These (Term syntax ann) -> a) -> SomeTermPair typeclasses ann -> a
withSomeTermPair with (SomeTermPair terms) = with terms

withParsedBlobPairs :: (Members '[Distribute WrappedTask, Exc SomeException, IO, Task, Telemetry] effs, Monoid output)
                    => (forall syntax . (ConstructorName syntax, Diffable syntax, Eq1 syntax, GAlign syntax, HasDeclaration syntax, IdentifierName syntax, Show1 syntax, ToJSONFields1 syntax, Traversable syntax) => Blob -> Term syntax (Record Location) -> TaskEff (Term syntax (Record fields)))
                    -> (forall syntax . (ConstructorName syntax, Diffable syntax, Eq1 syntax, GAlign syntax, HasDeclaration syntax, IdentifierName syntax, Show1 syntax, ToJSONFields1 syntax, Traversable syntax) => BlobPair -> Diff syntax (Record fields) (Record fields) -> TaskEff output)
                    -> [BlobPair]
                    -> Eff effs output
withParsedBlobPairs decorate render = distributeFoldMap (\ blobs -> WrapTask (withParsedBlobPair decorate blobs >>= withSomeTermPair (diffTerms blobs >=> render blobs)))
  where diffTerms :: (Diffable syntax, Eq1 syntax, GAlign syntax, Show1 syntax, Traversable syntax, Members '[IO, Task, Telemetry] effs) => BlobPair -> Join These (Term syntax (Record fields)) -> Eff effs (Diff syntax (Record fields) (Record fields))
        diffTerms blobs terms = time "diff" languageTag $ do
          diff <- diff (runJoin terms)
          diff <$ writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
          where languageTag = languageTagForBlobPair blobs

withParsedBlobPair :: Members '[Distribute WrappedTask, Exc SomeException, Task] effs
                   => (forall syntax . (ConstructorName syntax, Diffable syntax, Eq1 syntax, GAlign syntax, HasDeclaration syntax, IdentifierName syntax, Show1 syntax, ToJSONFields1 syntax, Traversable syntax) => Blob -> Term syntax (Record Location) -> TaskEff (Term syntax (Record fields)))
                   -> BlobPair
                   -> Eff effs (SomeTermPair '[ConstructorName, Diffable, Eq1, GAlign, HasDeclaration, IdentifierName, Show1, ToJSONFields1, Traversable] (Record fields))
withParsedBlobPair decorate blobs
  | Just (SomeParser parser) <- someParser @'[ConstructorName, Diffable, Eq1, GAlign, HasDeclaration, IdentifierName, Show1, ToJSONFields1, Traversable] <$> languageForBlobPair blobs
  = SomeTermPair <$> distributeFor blobs (\ blob -> WrapTask (parse parser blob >>= decorate blob))
  | otherwise = noLanguageForBlob (pathForBlobPair blobs)
