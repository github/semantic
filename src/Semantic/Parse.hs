{-# LANGUAGE GADTs #-}
module Semantic.Parse where

import Analysis.ConstructorName (ConstructorName, constructorLabel)
import Analysis.IdentifierName (IdentifierName, identifierLabel)
import Analysis.Declaration (HasDeclaration, declarationAlgebra)
import Analysis.PackageDef (HasPackageDef, packageDefAlgebra)
import Data.AST
import Data.Blob
import Data.JSON.Fields
import Data.Record
import Parsing.Parser
import Prologue hiding (MonadError(..))
import Rendering.Graph
import Rendering.Renderer
import Semantic.IO (noLanguageForBlob, FormatNotSupported(..))
import Semantic.Task
import Serializing.Format

parseBlobs :: (Members '[Distribute WrappedTask, Task, Exc SomeException] effs, Monoid output) => TermRenderer output -> [Blob] -> Eff effs output
parseBlobs renderer blobs = distributeFoldMap (WrapTask . parseBlob renderer) blobs

parseSomeBlob :: Members '[Task, Exc SomeException] effs => Blob -> Eff effs (SomeTerm '[ConstructorName, HasPackageDef, HasDeclaration, IdentifierName, Foldable, Functor, ToJSONFields1] (Record Location))
parseSomeBlob blob@Blob{..} = maybe (noLanguageForBlob blobPath) (flip parse blob . someParser) blobLanguage

renderSomeTerm :: Members '[Task, Exc SomeException] effs => TermRenderer output -> Blob -> SomeTerm '[ConstructorName, HasPackageDef, HasDeclaration, IdentifierName, Foldable, Functor, ToJSONFields1] (Record Location) -> Eff effs output
renderSomeTerm renderer blob@Blob{..} = withSomeTerm $ case renderer of
  JSONTermRenderer           -> decorate constructorLabel >=> decorate identifierLabel >=> render (renderJSONTerm blob)
  SExpressionTermRenderer    ->                                                            serialize SExpression
  TagsTermRenderer           -> decorate (declarationAlgebra blob)                     >=> render (renderToTags blob)
  ImportsTermRenderer        -> decorate (declarationAlgebra blob) >=> decorate (packageDefAlgebra blob) >=> render (renderToImports blob)
  SymbolsTermRenderer fields -> decorate (declarationAlgebra blob)                     >=> render (renderSymbolTerms . renderToSymbols fields blob)
  DOTTermRenderer            ->                                                            render renderTreeGraph >=> serialize (DOT (termStyle blobPath))


-- | A task to parse a 'Blob' and render the resulting 'Term'.
parseBlob :: Members '[Task, Exc SomeException] effs => TermRenderer output -> Blob -> Eff effs output
parseBlob renderer blob@Blob{..} = parseSomeBlob blob >>= renderSomeTerm renderer blob


astParseBlobs :: (Members '[Distribute WrappedTask, Task, Exc SomeException] effs, Monoid output) => TermRenderer output -> [Blob] -> Eff effs output
astParseBlobs renderer blobs = distributeFoldMap (WrapTask . astParseBlob renderer) blobs
  where
    astParseBlob :: Members '[Task, Exc SomeException] effs => TermRenderer output -> Blob -> Eff effs output
    astParseBlob renderer blob@Blob{..}
      | Just (SomeASTParser parser) <- someASTParser <$> blobLanguage
      = parse parser blob >>= case renderer of
        SExpressionTermRenderer    -> serialize SExpression
        JSONTermRenderer           -> render (renderJSONTerm' blob)
        _                          -> pure $ throwError (SomeException (FormatNotSupported "Only SExpression and JSON output supported for tree-sitter ASTs."))
      | otherwise = noLanguageForBlob blobPath
