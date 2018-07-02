{-# LANGUAGE GADTs, RankNTypes #-}
module Semantic.Parse where

import Analysis.ConstructorName (ConstructorName)
import Analysis.Declaration (HasDeclaration, declarationAlgebra)
import Analysis.PackageDef (HasPackageDef)
import Data.AST
import Data.Blob
import Data.JSON.Fields
import Data.Record
import Data.Term
import Parsing.Parser
import Data.Language hiding (JSON)
import Prologue hiding (MonadError(..))
import Rendering.Graph
import Rendering.Renderer
import Semantic.IO (noLanguageForBlob)
import Semantic.Task
import Serializing.Format
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TypeScript.Assignment as TypeScript
import qualified Language.JSON.Assignment as JSON

runParse :: (Member (Distribute WrappedTask) effs, Member Task effs) => TermRenderer output -> [Blob] -> Eff effs Builder
runParse JSONTermRenderer             = withParsedBlobs (render . renderJSONTerm) >=> serialize JSON
runParse SExpressionTermRenderer      = withParsedBlobs (const (serialize (SExpression ByConstructorName)))
runParse ShowTermRenderer             = withParsedBlobs (const (serialize Show))
runParse (SymbolsTermRenderer fields) = withParsedBlobs (\ blob -> decorate (declarationAlgebra blob) >=> render (renderSymbolTerms . renderToSymbols fields blob)) >=> serialize JSON
runParse DOTTermRenderer              = withParsedBlobs (const (render renderTreeGraph)) >=> serialize (DOT (termStyle "terms"))

runRubyParse :: Member (Distribute WrappedTask) effs => [Blob] -> Eff effs [Term (Sum Ruby.Syntax) ()]
runRubyParse = flip distributeFor (\ blob -> WrapTask (do
    term <- parse rubyParser blob
    pure (() <$ term)))

runTypeScriptParse :: Member (Distribute WrappedTask) effs => [Blob] -> Eff effs [Term (Sum TypeScript.Syntax) ()]
runTypeScriptParse = flip distributeFor (\ blob -> WrapTask (do
    term <- parse typescriptParser blob
    pure (() <$ term)))

runJSONParse :: Member (Distribute WrappedTask) effs => [Blob] -> Eff effs [Term (Sum JSON.Syntax) ()]
runJSONParse = flip distributeFor (\ blob -> WrapTask (do
    term <- parse jsonParser blob
    pure (() <$ term)))

withParsedBlobs :: (Member (Distribute WrappedTask) effs, Monoid output) => (forall syntax . (ConstructorName syntax, Foldable syntax, Functor syntax, HasDeclaration syntax, HasPackageDef syntax, Show1 syntax, ToJSONFields1 syntax) => Blob -> Term syntax (Record Location) -> TaskEff output) -> [Blob] -> Eff effs output
withParsedBlobs render = distributeFoldMap (\ blob -> WrapTask (parseSomeBlob blob >>= withSomeTerm (render blob)))

parseSomeBlob :: (Member (Exc SomeException) effs, Member Task effs) => Blob -> Eff effs (SomeTerm '[ConstructorName, Foldable, Functor, HasDeclaration, HasPackageDef, Show1, ToJSONFields1] (Record Location))
parseSomeBlob blob@Blob{..} = maybe (noLanguageForBlob blobPath) (`parse` blob) (someParser blobLanguage)
