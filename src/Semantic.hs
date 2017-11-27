{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveDataTypeable, GADTs, TypeOperators #-}
module Semantic
( parseBlobs
, parseBlob
, diffBlobPairs
, diffBlobPair
, diffTermPair
) where

import Analysis.Decorator
import Control.Exception
import Control.Monad ((>=>), guard)
import Control.Monad.Error.Class
import Data.Align.Generic
import Data.Bifoldable
import Data.Blob
import Data.ByteString (ByteString)
import Data.Diff
import Data.Functor.Both as Both
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


diffBlobPairs :: Output output => DiffRenderer output -> [Both Blob] -> Task ByteString
diffBlobPairs renderer = fmap toOutput . distributeFoldMap (diffBlobPair renderer) . filter (any blobExists)

-- | A task to parse a pair of 'Blob's, diff them, and render the 'Diff'.
diffBlobPair :: DiffRenderer output -> Both Blob -> Task output
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
  where (effectivePath, effectiveLanguage) = case runJoin blobs of
          (Blob { blobLanguage = Just lang, blobPath = path }, _) -> (path, Just lang)
          (_, Blob { blobLanguage = Just lang, blobPath = path }) -> (path, Just lang)
          (Blob { blobPath = path }, _)                           -> (path, Nothing)

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

        run :: (Foldable syntax, Functor syntax) => (Blob -> Task (Term syntax ann)) -> (Term syntax ann -> Term syntax ann -> Diff syntax ann ann) -> (Both Blob -> Diff syntax ann ann -> output) -> Task output
        run parse diff renderer = do
          terms <- distributeFor blobs parse
          time "diff" languageTag $ do
            diff <- runBothWith (diffTermPair blobs diff) terms
            writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
            render (renderer blobs) diff
          where
            showLanguage = pure . (,) "language" . show
            languageTag = let (a, b) = runJoin blobs
                          in maybe (maybe [] showLanguage (blobLanguage b)) showLanguage (blobLanguage a)

-- | A task to diff a pair of 'Term's, producing insertion/deletion 'Patch'es for non-existent 'Blob's.
diffTermPair :: Functor syntax => Both Blob -> Differ syntax ann1 ann2 -> Term syntax ann1 -> Term syntax ann2 -> Task (Diff syntax ann1 ann2)
diffTermPair blobs differ t1 t2 = case runJoin (blobExists <$> blobs) of
  (True, False) -> pure (deleting t1)
  (False, True) -> pure (inserting t2)
  _ -> diff differ t1 t2

keepCategory :: HasField fields Category => Record fields -> Record '[Category]
keepCategory = (:. Nil) . category
