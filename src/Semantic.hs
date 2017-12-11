{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveDataTypeable, GADTs, TypeOperators #-}
module Semantic
( parseBlobs
, parseBlob
, diffBlobPairs
, diffBlobPair
, diffTermPair
) where

import Analysis.ConstructorName (ConstructorName, constructorLabel)
import Analysis.Declaration (HasDeclaration, declarationAlgebra, syntaxDeclarationAlgebra)
import Analysis.Decorator (syntaxIdentifierAlgebra)
import Control.Exception
import Control.Monad ((>=>), guard)
import Control.Monad.Error.Class
import Data.Align.Generic
import Data.Bifoldable
import Data.Blob
import Data.ByteString (ByteString)
import Data.Diff
import Data.Functor.Classes
import Data.JSON.Fields
import qualified Data.Language as Language
import Data.Output
import Data.Record
import Data.Term
import Data.Typeable
import Diffing.Algorithm (Diffable)
import Diffing.Interpreter
import Info
import Parsing.Parser
import Rendering.Renderer
import Semantic.Stat as Stat
import Semantic.Task as Task

-- This is the primary interface to the Semantic library which provides two
-- major classes of functionality: semantic parsing and diffing of source code
-- blobs.
--
-- Design goals:
--   - No knowledge of the filesystem or Git.
--   - Built in concurrency where appropriate.
--   - Easy to consume this interface from other application (e.g a cmdline or web server app).

parseBlobs :: Output output => TermRenderer output -> [Blob] -> Task ByteString
parseBlobs renderer = fmap toOutput . distributeFoldMap (parseBlob renderer) . filter blobExists

-- | A task to parse a 'Blob' and render the resulting 'Term'.
parseBlob :: TermRenderer output -> Blob -> Task output
parseBlob renderer blob@Blob{..}
  | Just (SomeParser parser) <- blobLanguage >>= someParser (Proxy :: Proxy '[ConstructorName, HasDeclaration, Foldable, Functor, ToJSONFields1])
  = parse parser blob >>= case renderer of
    ToCTermRenderer         -> decorate (declarationAlgebra blob)   >=> render (renderToCTerm  blob)
    JSONTermRenderer        -> decorate constructorLabel            >=> render (renderJSONTerm blob)
    SExpressionTermRenderer -> decorate constructorLabel . (Nil <$) >=> render renderSExpressionTerm
    TagsTermRenderer        -> decorate (declarationAlgebra blob)   >=> render (renderToTags blob)

  | Just parser <- blobLanguage >>= syntaxParserForLanguage
  = parse parser blob >>= case renderer of
    ToCTermRenderer         -> decorate (syntaxDeclarationAlgebra blob) >=> render (renderToCTerm blob)
    JSONTermRenderer        -> decorate syntaxIdentifierAlgebra         >=> render (renderJSONTerm blob)
    SExpressionTermRenderer ->                                              render renderSExpressionTerm . fmap keepCategory
    TagsTermRenderer        -> decorate (syntaxDeclarationAlgebra blob) >=> render (renderToTags blob)

  | otherwise = throwError (SomeException (NoParserForLanguage blobPath blobLanguage))

data NoParserForLanguage = NoParserForLanguage FilePath (Maybe Language.Language)
  deriving (Eq, Exception, Ord, Show, Typeable)


diffBlobPairs :: Output output => DiffRenderer output -> [BlobPair] -> Task ByteString
diffBlobPairs renderer = fmap toOutput . distributeFoldMap (diffBlobPair renderer)

-- | A task to parse a pair of 'Blob's, diff them, and render the 'Diff'.
diffBlobPair :: DiffRenderer output -> BlobPair -> Task output
diffBlobPair renderer blobs
  | Just (SomeParser parser) <- effectiveLanguage >>= qualify >>= someParser (Proxy :: Proxy '[ConstructorName, Diffable, Eq1, GAlign, HasDeclaration, Show1, ToJSONFields1, Traversable])
  = case renderer of
    OldToCDiffRenderer      -> run (\ blob -> parse parser blob >>= decorate (declarationAlgebra blob))   diffTerms renderToCDiff
    ToCDiffRenderer         -> run (\ blob -> parse parser blob >>= decorate (declarationAlgebra blob))   diffTerms renderToCDiff
    JSONDiffRenderer        -> run (          parse parser)                                               diffTerms renderJSONDiff
    SExpressionDiffRenderer -> run (          parse parser      >=> decorate constructorLabel . (Nil <$)) diffTerms (const renderSExpressionDiff)

  | Just parser <- effectiveLanguage >>= syntaxParserForLanguage
  = case renderer of
    OldToCDiffRenderer      -> run (\ blob -> parse parser blob >>= decorate (syntaxDeclarationAlgebra blob)) diffSyntaxTerms renderToCDiff
    ToCDiffRenderer         -> run (\ blob -> parse parser blob >>= decorate (syntaxDeclarationAlgebra blob)) diffSyntaxTerms renderToCDiff
    JSONDiffRenderer        -> run (          parse parser      >=> decorate syntaxIdentifierAlgebra)         diffSyntaxTerms renderJSONDiff
    SExpressionDiffRenderer -> run (          parse parser      >=> pure . fmap keepCategory)                 diffSyntaxTerms (const renderSExpressionDiff)

  | otherwise = throwError (SomeException (NoParserForLanguage effectivePath effectiveLanguage))
  where effectiveLanguage = languageForBlobPair blobs
        effectivePath = pathForBlobPair blobs

        qualify language | OldToCDiffRenderer <- renderer = guard (language `elem` aLaCarteLanguages) *> Just language
                         | otherwise                      =                                              Just language
        aLaCarteLanguages
          = [ Language.JSX
            , Language.JavaScript
            , Language.Markdown
            , Language.Python
            , Language.Ruby
            , Language.TypeScript
            ]

        run :: (Foldable syntax, Functor syntax) => (Blob -> Task (Term syntax ann)) -> (Term syntax ann -> Term syntax ann -> Diff syntax ann ann) -> (These Blob Blob -> Diff syntax ann ann -> output) -> Task output
        run parse diff renderer = do
          terms <- bidistributeFor blobs parse parse
          time "diff" languageTag $ do
            diff <- diffTermPair diff terms
            writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
            render (renderer blobs) diff
          where
            languageTag = languageTagForBlobPair blobs

-- | A task to diff 'Term's, producing insertion/deletion 'Patch'es for non-existent 'Blob's.
diffTermPair :: Functor syntax => Differ syntax ann1 ann2 -> These (Term syntax ann1) (Term syntax ann2) -> Task (Diff syntax ann1 ann2)
diffTermPair _      (This t1)     = pure (deleting t1)
diffTermPair _      (That t2)     = pure (inserting t2)
diffTermPair differ (These t1 t2) = diff differ t1 t2

keepCategory :: HasField fields Category => Record fields -> Record '[Category]
keepCategory = (:. Nil) . category
