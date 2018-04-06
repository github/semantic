{-# LANGUAGE GADTs #-}
module Semantic.Parse where

import Analysis.ConstructorName (ConstructorName, constructorLabel)
import Analysis.IdentifierName (IdentifierName, identifierLabel)
import Analysis.Declaration (HasDeclaration, declarationAlgebra)
import Analysis.PackageDef (HasPackageDef, packageDefAlgebra)
import qualified Data.Abstract.Evaluatable as Analysis
import Data.Abstract.FreeVariables
import Data.Blob
import Data.JSON.Fields
import Data.Output
import Data.Record
import Parsing.Parser
import Prologue hiding (MonadError(..))
import Rendering.Renderer
import Semantic.IO (NoLanguageForBlob(..), Files)
import Semantic.Task
import System.FilePath.Posix
import Data.ByteString.Char8 as BC (pack)


graph :: (Members '[Distribute WrappedTask, Files, Task, Exc SomeException] effs) => TermRenderer output -> Blob -> Eff effs ByteString
graph _ Blob{..}
  | Just (SomeAnalysisParser parser exts) <- someAnalysisParser
    (Proxy :: Proxy '[ Analysis.Evaluatable, FreeVariables1, Functor, Eq1, Ord1, Show1 ]) <$> blobLanguage = do
    let rootDir = takeDirectory blobPath
    paths <- filter (/= blobPath) <$> listFiles rootDir exts
    package <- parsePackage (packageName blobPath) parser rootDir (blobPath : paths)
    graphImports package
  | otherwise = throwError (SomeException (NoLanguageForBlob blobPath))

  where packageName = name . BC.pack . dropExtensions . takeFileName


parseBlobs :: (Members '[Distribute WrappedTask, Task, Exc SomeException] effs, Output output) => TermRenderer output -> [Blob] -> Eff effs ByteString
parseBlobs renderer blobs = toOutput' <$> distributeFoldMap (WrapTask . parseBlob renderer) blobs
  where toOutput' = case renderer of
          JSONTermRenderer -> toOutput . renderJSONTerms
          SymbolsTermRenderer _ -> toOutput . renderSymbolTerms
          _ -> toOutput

-- | A task to parse a 'Blob' and render the resulting 'Term'.
parseBlob :: Members '[Task, Exc SomeException] effs => TermRenderer output -> Blob -> Eff effs output
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
