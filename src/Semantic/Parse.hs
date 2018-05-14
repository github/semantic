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

parseBlobs :: (Members '[Distribute WrappedTask, Task, Exc SomeException] effs, Monoid output) => TermRenderer output -> [Blob] -> Eff effs output
parseBlobs renderer = distributeFoldMap (WrapTask . parseBlob renderer)

parseSomeBlob :: Members '[Task, Exc SomeException] effs => Blob -> Eff effs (SomeTerm '[ConstructorName, HasPackageDef, HasDeclaration, IdentifierName, Foldable, Functor, ToJSONFields1] (Record Location))
parseSomeBlob blob@Blob{..} = maybe (noLanguageForBlob blobPath) (flip parse blob . someParser) blobLanguage

renderTerm :: (ConstructorName syntax, HasPackageDef syntax, HasDeclaration syntax, IdentifierName syntax, Foldable syntax, Functor syntax, ToJSONFields1 syntax, Members '[Task, Exc SomeException] effs) => TermRenderer output -> Blob -> Term syntax (Record Location) -> Eff effs output
renderTerm renderer blob@Blob{..} = case renderer of
  JSONTermRenderer           -> decorate constructorLabel >=> decorate identifierLabel >=> render (renderJSONTerm blob)
  SExpressionTermRenderer    ->                                                            serialize (SExpression ByConstructorName)
  TagsTermRenderer           -> decorate (declarationAlgebra blob)                     >=> render (renderToTags blob)
  ImportsTermRenderer        -> decorate (declarationAlgebra blob) >=> decorate (packageDefAlgebra blob) >=> render (renderToImports blob)
  SymbolsTermRenderer fields -> decorate (declarationAlgebra blob)                     >=> render (renderSymbolTerms . renderToSymbols fields blob)
  DOTTermRenderer            ->                                                            render renderTreeGraph >=> serialize (DOT (termStyle blobPath))


-- | A task to parse a 'Blob' and render the resulting 'Term'.
parseBlob :: Members '[Task, Exc SomeException] effs => TermRenderer output -> Blob -> Eff effs output
parseBlob renderer blob@Blob{..} = parseSomeBlob blob >>= withSomeTerm (renderTerm renderer blob)
