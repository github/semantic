{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, RecordWildCards, ScopedTypeVariables, TupleSections, TypeApplications, TypeFamilies, UndecidableInstances #-}
module Semantic.Api.TOCSummaries
( diffSummary
, legacyDiffSummary
, diffSummaryBuilder
, SummarizeTerms(..)
, summarizeTermParsers
) where

import           Analysis.Decorator (decoratorWithAlgebra)
import           Analysis.TOCSummary (Declaration(..), HasDeclaration, Kind(..), declarationAlgebra, formatKind)
import           Control.Applicative (liftA2)
import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Edit
import           Data.Either (partitionEithers)
import           Data.Function (on)
import           Data.Functor.Foldable (Base, Recursive)
import           Data.Language (Language, LanguageMode(..), PerLanguageModes)
import           Data.Map (Map)
import qualified Data.Map.Monoidal as Map
import           Data.Maybe (mapMaybe)
import           Data.ProtoLens (defMessage)
import           Data.Semilattice.Lower
import           Data.Term (IsTerm(..), TermF)
import qualified Data.Text as T
import qualified Diffing.Algorithm.SES as SES
import           Diffing.Interpreter (DiffTerms)
import           Parsing.Parser (SomeParser, TermMode, allParsers)
import           Proto.Semantic as P hiding (Blob, BlobPair)
import           Proto.Semantic_Fields as P
import           Rendering.TOC
import           Semantic.Api.Bridge
import           Semantic.Api.Diffs
import           Semantic.Config (Config)
import           Semantic.Task as Task
import           Serializing.Format
import           Source.Loc as Loc
import           Source.Source as Source
import qualified Tags.Tag as Tag
import qualified Tags.Tagging.Precise as Tagging

diffSummaryBuilder :: (Carrier sig m, Member Distribute sig, Member (Error SomeException) sig, Member Parse sig, Member (Reader Config) sig, Member (Reader PerLanguageModes) sig, Member Telemetry sig, MonadIO m) => Format DiffTreeTOCResponse -> [BlobPair] -> m Builder
diffSummaryBuilder format blobs = diffSummary blobs >>= serialize format

legacyDiffSummary :: (Carrier sig m, Member Distribute sig, Member (Error SomeException) sig, Member Parse sig, Member (Reader PerLanguageModes) sig, Member Telemetry sig, MonadIO m) => [BlobPair] -> m Summaries
legacyDiffSummary = distributeFoldMap go
  where
    go :: (Carrier sig m, Member (Error SomeException) sig, Member Parse sig, Member (Reader PerLanguageModes) sig, Member Telemetry sig, MonadIO m) => BlobPair -> m Summaries
    go blobPair = asks summarizeTermParsers >>= \ p -> parsePairWith p (fmap (uncurry (flip Summaries) . bimap toMap toMap . partitionEithers) . summarizeTerms) blobPair
      `catchError` \(SomeException e) ->
        pure $ Summaries mempty (toMap [ErrorSummary (T.pack (show e)) lowerBound lang])
      where path = T.pack $ pathKeyForBlobPair blobPair
            lang = languageForBlobPair blobPair

            toMap :: ToJSON a => [a] -> Map.Map T.Text [Value]
            toMap [] = mempty
            toMap as = Map.singleton path (toJSON <$> as)


diffSummary :: (Carrier sig m, Member Distribute sig, Member (Error SomeException) sig, Member Parse sig, Member (Reader PerLanguageModes) sig, Member Telemetry sig, MonadIO m) => [BlobPair] -> m DiffTreeTOCResponse
diffSummary blobs = do
  diff <- distributeFor blobs go
  pure $ defMessage & P.files .~ diff
  where
    go :: (Carrier sig m, Member (Error SomeException) sig, Member Parse sig, Member (Reader PerLanguageModes) sig, Member Telemetry sig, MonadIO m) => BlobPair -> m TOCSummaryFile
    go blobPair = asks summarizeTermParsers >>= \ p -> parsePairWith p (fmap (uncurry toFile . partitionEithers . map (bimap toError toChange)) . summarizeTerms) blobPair
      `catchError` \(SomeException e) ->
        pure $ toFile [defMessage & P.error .~ T.pack (show e) & P.maybe'span .~ Nothing] []
      where toFile errors changes = defMessage
              & P.path     .~ T.pack (pathKeyForBlobPair blobPair)
              & P.language .~ bridging # languageForBlobPair blobPair
              & P.changes  .~ changes
              & P.errors   .~ errors

toChangeType :: Change -> ChangeType
toChangeType = \case
  Changed  -> MODIFIED
  Deleted  -> REMOVED
  Inserted -> ADDED
  Replaced -> MODIFIED

toChange :: TOCSummary -> TOCSummaryChange
toChange TOCSummary{..} = defMessage
  & P.category   .~ formatKind kind
  & P.term       .~ ident
  & P.maybe'span ?~ converting # span
  & P.changeType .~ toChangeType change

toError :: ErrorSummary -> TOCSummaryError
toError ErrorSummary{..} = defMessage
  & P.error      .~ message
  & P.maybe'span ?~ converting # span


summarizeTermParsers :: PerLanguageModes -> Map Language (SomeParser SummarizeTerms Loc)
summarizeTermParsers = allParsers

class SummarizeTerms term where
  summarizeTerms :: (Member Telemetry sig, Carrier sig m, MonadIO m) => Edit (Blob, term Loc) (Blob, term Loc) -> m [Either ErrorSummary TOCSummary]

instance (TermMode term ~ strategy, SummarizeTermsBy strategy term) => SummarizeTerms term where
  summarizeTerms = summarizeTermsBy @strategy

class SummarizeTermsBy (strategy :: LanguageMode) term where
  summarizeTermsBy :: (Member Telemetry sig, Carrier sig m, MonadIO m) => Edit (Blob, term Loc) (Blob, term Loc) -> m [Either ErrorSummary TOCSummary]

instance (DiffTerms term, HasDeclaration (Syntax term), Traversable (Syntax term), Recursive (term Loc), Base (term Loc) ~ TermF (Syntax term) Loc) => SummarizeTermsBy 'ALaCarte term where
  summarizeTermsBy = fmap diffTOC . diffTerms . bimap decorateTerm decorateTerm where
    decorateTerm :: (Blob, term Loc) -> (Blob, term (Maybe Declaration))
    decorateTerm (blob, term) = (blob, decoratorWithAlgebra (declarationAlgebra blob) term)


instance Tagging.ToTags term => SummarizeTermsBy 'Precise term where
  summarizeTermsBy terms = pure . map (uncurry summarizeChange) . dedupe . mapMaybe toChange . edit (map Delete) (map Insert) (SES.ses compare) . bimap (uncurry go) (uncurry go) $ terms where
    go = Tagging.tags . blobSource
    lang = languageForBlobPair (bimap fst fst terms)
    (s1, s2) = edit (,mempty) (mempty,) (,) (bimap (blobSource . fst) (blobSource . fst) terms)
    compare = liftA2 (&&) <$> ((==) `on` Tag.kind) <*> ((==) `on` Tag.name)

    toChange = \case
      Delete tag -> (Deleted,)  <$> toDecl tag
      Insert tag -> (Inserted,) <$> toDecl tag
      Compare t1 t2
        | Source.slice s1 (byteRange (Tag.loc t1)) /= Source.slice s2 (byteRange (Tag.loc t2))
                     -> (Changed,) <$> toDecl t2
        | otherwise  -> Nothing

    toDecl (Tag.Tag name kind loc _ _) = do
      kind <- toKind kind
      pure (Declaration kind name (Loc.span loc) lang)

    toKind = \case
      Tag.Function -> Just Function
      Tag.Method   -> Just (Method Nothing)
      _            -> Nothing
