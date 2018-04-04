{-# LANGUAGE GADTs #-}
module Semantic.Command.Parse where

import Analysis.ConstructorName (ConstructorName, constructorLabel)
import Analysis.IdentifierName (IdentifierName, identifierLabel)
import Analysis.Declaration (HasDeclaration, declarationAlgebra)
import Analysis.PackageDef (HasPackageDef, packageDefAlgebra)
import Data.Blob
import Data.JSON.Fields
import Data.Output
import Data.Record
import Parsing.Parser
import Prologue hiding (MonadError(..))
import Rendering.Renderer
import Semantic.IO (NoLanguageForBlob(..))
import Semantic.Task

parseBlobs :: Output output => TermRenderer output -> [Blob] -> Task ByteString
parseBlobs renderer blobs = toOutput' <$> distributeFoldMap (WrapTask . parseBlob renderer) blobs
  where toOutput' = case renderer of
          JSONTermRenderer -> toOutput . renderJSONTerms
          SymbolsTermRenderer _ -> toOutput . renderSymbolTerms
          _ -> toOutput

-- | A task to parse a 'Blob' and render the resulting 'Term'.
parseBlob :: TermRenderer output -> Blob -> Task output
parseBlob renderer blob@Blob{..}
  | Just (SomeParser parser) <- someParser (Proxy :: Proxy '[ConstructorName, HasPackageDef, HasDeclaration, IdentifierName, Foldable, Functor, ToJSONFields1]) <$> blobLanguage
  = parse parser blob >>= case renderer of
    JSONTermRenderer           -> decorate constructorLabel >=> decorate identifierLabel >=> render (renderJSONTerm blob)
    SExpressionTermRenderer    -> decorate constructorLabel . (Nil <$)                   >=> render renderSExpressionTerm
    TagsTermRenderer           -> decorate (declarationAlgebra blob)                     >=> render (renderToTags blob)
    ImportsTermRenderer        -> decorate (declarationAlgebra blob) >=> decorate (packageDefAlgebra blob) >=> render (renderToImports blob)
    SymbolsTermRenderer fields -> decorate (declarationAlgebra blob)                     >=> render (renderToSymbols fields blob)
    DOTTermRenderer            ->                                                            render (renderDOTTerm blob)
  | otherwise = throwError (SomeException (NoLanguageForBlob blobPath))
