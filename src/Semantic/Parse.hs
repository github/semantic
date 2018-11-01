{-# LANGUAGE GADTs, RankNTypes #-}
module Semantic.Parse ( runParse, runParse', parseSomeBlob ) where

import           Analysis.ConstructorName (ConstructorName)
import           Analysis.Declaration (HasDeclaration, declarationAlgebra)
import           Analysis.PackageDef (HasPackageDef)
import           Control.Effect
import           Control.Effect.Error
import           Control.Monad.IO.Class
import           Data.Blob
import           Data.Either
import           Data.Abstract.Declarations
import           Data.ByteString.Builder (stringUtf8)
import           Data.Graph.TermVertex
import           Data.JSON.Fields
import           Data.Quieterm
import           Data.Location
import           Data.Term
import           Parsing.Parser
import           Prologue
import           Rendering.Graph
import           Rendering.JSON (SomeJSON (..))
import qualified Rendering.JSON as JSON
import           Rendering.Renderer
import           Semantic.Task
import           Serializing.Format
import Tags.Taggable

-- | Using the specified renderer, parse a list of 'Blob's to produce a 'Builder' output.
runParse :: (Member Distribute sig, Member (Error SomeException) sig, Member Task sig, Carrier sig m, MonadIO m) => TermRenderer output -> [Blob] -> m Builder
runParse JSONTermRenderer             = withParsedBlobs' renderJSONError (render . renderJSONTerm) >=> serialize JSON
runParse JSONGraphTermRenderer        = withParsedBlobs' renderJSONError (render . renderAdjGraph) >=> serialize JSON
  where renderAdjGraph :: (Recursive t, ToTreeGraph TermVertex (Base t)) => Blob -> t -> JSON.JSON "trees" SomeJSON
        renderAdjGraph blob term = renderJSONAdjTerm blob (renderTreeGraph term)
runParse SExpressionTermRenderer      = withParsedBlobs (const (serialize (SExpression ByConstructorName)))
runParse ShowTermRenderer             = withParsedBlobs (const (serialize Show . quieterm))
runParse (SymbolsTermRenderer fields) = withParsedBlobs (\ blob -> render (renderSymbolTerms . renderToSymbols' fields blob)) >=> serialize JSON
-- runParse (SymbolsTermRenderer fields) = withParsedBlobs (\ blob -> decorate (declarationAlgebra blob) >=> render (renderSymbolTerms . renderToSymbols fields blob)) >=> serialize JSON
runParse DOTTermRenderer              = withParsedBlobs (const (render renderTreeGraph)) >=> serialize (DOT (termStyle "terms"))
runParse QuietTermRenderer            = distributeFoldMap $ \blob ->
  showTiming blob <$> time' ((parseSomeBlob blob >>= withSomeTerm (fmap (const (Right ())) . serialize Show . quieterm)) `catchError` \(SomeException e) -> pure (Left (show e)))
  where
    showTiming Blob{..} (res, duration) =
      let status = if isLeft res then "ERR" else "OK"
      in stringUtf8 (status <> "\t" <> show blobLanguage <> "\t" <> blobPath <> "\t" <> show duration <> " ms\n")

-- | For testing and running parse-examples.
runParse' :: (Member (Error SomeException) sig, Member Task sig, Monad m, Carrier sig m) => Blob -> m Builder
runParse' blob = parseSomeBlob blob >>= withSomeTerm (serialize Show . quieterm)

type Render m output
  = forall syntax
  .  ( ConstructorName syntax
     , HasDeclaration syntax
     , HasPackageDef syntax
     , Foldable syntax
     , Functor syntax
     , Show1 syntax
     , ToJSONFields1 syntax
     , Declarations1 syntax
     , Taggable syntax
     , HasTextElement syntax
     )
  => Blob
  -> Term syntax Location
  -> m output

withParsedBlobs :: (Member Distribute sig, Member (Error SomeException) sig, Member Task sig, Monad m, Monoid output, Carrier sig m)
  => Render m output -> [Blob] -> m output
withParsedBlobs render = distributeFoldMap $ \blob -> parseSomeBlob blob >>= withSomeTerm (render blob)

withParsedBlobs' :: (Member Distribute sig, Member (Error SomeException) sig, Member Task sig, Monad m, Monoid output, Carrier sig m)
  => (Blob -> String -> output) -> Render m output -> [Blob] -> m output
withParsedBlobs' onError render = distributeFoldMap $ \blob ->
  (parseSomeBlob blob >>= withSomeTerm (render blob)) `catchError` \(SomeException e) ->
    pure (onError blob (show e))

parseSomeBlob :: (Member (Error SomeException) sig, Member Task sig, Carrier sig m) => Blob -> m (SomeTerm '[ConstructorName, Foldable, Functor, HasDeclaration, HasPackageDef, Show1, ToJSONFields1, Taggable, HasTextElement, Declarations1] Location)
parseSomeBlob blob@Blob{..} = maybe (noLanguageForBlob blobPath) (`parse` blob) (someParser blobLanguage)
