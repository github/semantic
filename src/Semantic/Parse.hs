{-# LANGUAGE GADTs, RankNTypes #-}
module Semantic.Parse ( runParse, runParse' ) where

import           Analysis.ConstructorName (ConstructorName)
import           Analysis.Declaration (HasDeclaration, declarationAlgebra)
import           Analysis.PackageDef (HasPackageDef)
import           Control.Monad.Effect.Exception
import           Data.AST
import           Data.Blob
import           Data.Graph.TermVertex
import           Data.JSON.Fields
import           Data.Quieterm
import           Data.Record
import           Data.Term
import           Parsing.Parser
import           Prologue hiding (MonadError (..))
import           Rendering.Graph
import           Rendering.JSON (SomeJSON (..))
import qualified Rendering.JSON as JSON
import           Rendering.Renderer
import           Semantic.IO (noLanguageForBlob)
import           Semantic.Task
import           Serializing.Format

runParse :: (Member Distribute effs, Member (Exc SomeException) effs, Member Task effs) => TermRenderer output -> [Blob] -> Eff effs Builder
runParse JSONTermRenderer             = withParsedBlobs renderJSONError (render . renderJSONTerm) >=> serialize JSON
runParse JSONGraphTermRenderer          = withParsedBlobs renderJSONError (render . renderAdjGraph) >=> serialize JSON
  where renderAdjGraph :: (Recursive t, ToTreeGraph TermVertex (Base t)) => Blob -> t -> JSON.JSON "trees" SomeJSON
        renderAdjGraph blob term = renderJSONAdjTerm blob (renderTreeGraph term)
runParse SExpressionTermRenderer      = withParsedBlobs (\_ _ -> mempty) (const (serialize (SExpression ByConstructorName)))
runParse ShowTermRenderer             = withParsedBlobs (\_ _ -> mempty) (const (serialize Show . quieterm))
runParse (SymbolsTermRenderer fields) = withParsedBlobs (\_ _ -> mempty) (\ blob -> decorate (declarationAlgebra blob) >=> render (renderSymbolTerms . renderToSymbols fields blob)) >=> serialize JSON
runParse DOTTermRenderer              = withParsedBlobs (\_ _ -> mempty) (const (render renderTreeGraph)) >=> serialize (DOT (termStyle "terms"))


runParse' :: (Member (Exc SomeException) effs, Member Task effs) => Blob -> Eff effs Builder
runParse' blob = parseSomeBlob blob >>= withSomeTerm (serialize Show . quieterm)

withParsedBlobs ::
  ( Member Distribute effs
  , Member (Exc SomeException) effs
  , Member Task effs
  , Monoid output
  )
  => (Blob -> String -> output)
  -> ( forall syntax .
        ( ConstructorName syntax
        , Foldable syntax
        , Functor syntax
        , HasDeclaration syntax
        , HasPackageDef syntax
        , Show1 syntax
        , ToJSONFields1 syntax
        ) => Blob -> Term syntax (Record Location) -> Eff effs output
      )
  -> [Blob]
  -> Eff effs output
withParsedBlobs onError render = distributeFoldMap $ \blob ->
  (parseSomeBlob blob >>= withSomeTerm (render blob)) `catchError` \(SomeException e) ->
    pure (onError blob (show e))

parseSomeBlob :: (Member (Exc SomeException) effs, Member Task effs) => Blob -> Eff effs (SomeTerm '[ConstructorName, Foldable, Functor, HasDeclaration, HasPackageDef, Show1, ToJSONFields1] (Record Location))
parseSomeBlob blob@Blob{..} = maybe (noLanguageForBlob blobPath) (`parse` blob) (someParser blobLanguage)
