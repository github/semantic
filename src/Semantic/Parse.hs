{-# LANGUAGE GADTs, RankNTypes #-}
module Semantic.Parse where

import Analysis.ConstructorName (ConstructorName, constructorLabel)
import Analysis.IdentifierName (IdentifierName, identifierLabel)
import Analysis.Declaration (HasDeclaration, declarationAlgebra)
import Analysis.PackageDef (HasPackageDef, packageDefAlgebra)
import Data.AST
import Data.Blob
import Data.JSON.Fields
import Data.Record
import Data.Term
import Parsing.Parser
import Prologue hiding (MonadError(..))
import Rendering.Graph
import Rendering.Renderer
import Semantic.IO (noLanguageForBlob)
import Semantic.Task
import Serializing.Format

runParse :: (Members '[Distribute WrappedTask, Task, Exc SomeException] effs, Monoid output) => TermRenderer output -> [Blob] -> Eff effs Builder
runParse JSONTermRenderer             = withParsedBlob (\ blob -> decorate constructorLabel >=> decorate identifierLabel >=> render (renderJSONTerm blob)) >=> serialize JSON
runParse SExpressionTermRenderer      = withParsedBlob (const (serialize (SExpression ByConstructorName)))
runParse TagsTermRenderer             = withParsedBlob (\ blob -> decorate (declarationAlgebra blob) >=> render (renderToTags blob)) >=> serialize JSON
runParse ImportsTermRenderer          = withParsedBlob (\ blob -> decorate (declarationAlgebra blob) >=> decorate (packageDefAlgebra blob) >=> render (renderToImports blob)) >=> serialize JSON
runParse (SymbolsTermRenderer fields) = withParsedBlob (\ blob -> decorate (declarationAlgebra blob) >=> render (renderSymbolTerms . renderToSymbols fields blob)) >=> serialize JSON
runParse DOTTermRenderer              = withParsedBlob (const (render renderTreeGraph)) >=> serialize (DOT (termStyle "terms"))

withParsedBlob :: (Members '[Distribute WrappedTask, Task, Exc SomeException] effs, Monoid output) => (forall syntax . (ConstructorName syntax, HasPackageDef syntax, HasDeclaration syntax, IdentifierName syntax, Foldable syntax, Functor syntax, ToJSONFields1 syntax) => Blob -> Term syntax (Record Location) -> TaskEff output) -> [Blob] -> Eff effs output
withParsedBlob render = distributeFoldMap (\ blob -> WrapTask (parseSomeBlob blob >>= withSomeTerm (render blob)))

parseSomeBlob :: Members '[Task, Exc SomeException] effs => Blob -> Eff effs (SomeTerm '[ConstructorName, HasPackageDef, HasDeclaration, IdentifierName, Foldable, Functor, ToJSONFields1] (Record Location))
parseSomeBlob blob@Blob{..} = maybe (noLanguageForBlob blobPath) (flip parse blob . someParser) blobLanguage
