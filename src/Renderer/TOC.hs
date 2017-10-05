{-# LANGUAGE DataKinds, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Renderer.TOC
( renderToCDiff
, renderToCTerm
, diffTOC
, Summaries(..)
, JSONSummary(..)
, isValidSummary
, Declaration(..)
, declaration
, declarationAlgebra
, markupSectionAlgebra
, toDeclarationAlgebra
, syntaxDeclarationAlgebra
, Entry(..)
, tableOfContentsBy
, dedupe
, entrySummary
) where

import Data.Aeson
import Data.Align (bicrosswalk)
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (bimap)
import Data.Blob
import Data.ByteString.Lazy (toStrict)
import Data.Diff
import Data.Error as Error (Error(..), showExpectation)
import Data.Foldable (fold, foldl', toList)
import Data.Functor.Both hiding (fst, snd)
import Data.Functor.Foldable (cata)
import Data.Function (on)
import Data.List.NonEmpty (nonEmpty)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Output
import Data.Patch
import Data.Proxy
import Data.Record
import Data.Semigroup ((<>), sconcat)
import Data.Source as Source
import Data.Term
import Data.Text (toLower)
import qualified Data.Text as T
import Data.Union
import GHC.Generics
import Info
import Language
import qualified Data.List as List
import qualified Data.Map as Map hiding (null)
import Syntax as S
import Data.Syntax.Algebra (RAlgebra)
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Markup as Markup

data Summaries = Summaries { changes, errors :: !(Map.Map T.Text [Value]) }
  deriving (Eq, Show)

instance Monoid Summaries where
  mempty = Summaries mempty mempty
  mappend (Summaries c1 e1) (Summaries c2 e2) = Summaries (Map.unionWith (<>) c1 c2) (Map.unionWith (<>) e1 e2)

instance Output Summaries where
  toOutput = toStrict . (<> "\n") . encode

instance ToJSON Summaries where
  toJSON Summaries{..} = object [ "changes" .= changes, "errors" .= errors ]

data JSONSummary
  = JSONSummary
    { summaryCategoryName :: T.Text
    , summaryTermName :: T.Text
    , summarySpan :: Span
    , summaryChangeType :: T.Text
    }
  | ErrorSummary { error :: T.Text, errorSpan :: Span, errorLanguage :: Maybe Language }
  deriving (Generic, Eq, Show)

instance ToJSON JSONSummary where
  toJSON JSONSummary{..} = object [ "changeType" .= summaryChangeType, "category" .= summaryCategoryName, "term" .= summaryTermName, "span" .= summarySpan ]
  toJSON ErrorSummary{..} = object [ "error" .= error, "span" .= errorSpan, "language" .= errorLanguage ]

isValidSummary :: JSONSummary -> Bool
isValidSummary ErrorSummary{} = False
isValidSummary _ = True

-- | A declaration’s identifier and type.
data Declaration
  = MethodDeclaration   { declarationIdentifier :: T.Text }
  | FunctionDeclaration { declarationIdentifier :: T.Text }
  | SectionDeclaration  { declarationIdentifier :: T.Text, declarationLevel :: Int }
  | ErrorDeclaration    { declarationIdentifier :: T.Text, declarationLanguage :: Maybe Language }
  deriving (Eq, Generic, Show)


class HasDeclaration whole part where
  toDeclaration :: (HasField fields Range, HasField fields Span) => Blob -> Record fields -> RAlgebra part (Term whole (Record fields)) (Maybe Declaration)

class HasDeclaration' whole part where
  toDeclaration' :: (HasField fields Range, HasField fields Span) => Blob -> Record fields -> RAlgebra part (Term whole (Record fields)) (Maybe Declaration)

toDeclarationAlgebra :: (HasField fields Range, HasField fields Span, HasDeclaration syntax syntax) => Blob -> RAlgebra (TermF syntax (Record fields)) (Term syntax (Record fields)) (Maybe Declaration)
toDeclarationAlgebra blob (In ann syntax) = toDeclaration blob ann syntax


instance (DeclarationStrategy part ~ strategy, HasDeclarationWithStrategy strategy whole part) => HasDeclaration whole part where
  toDeclaration = toDeclarationWithStrategy (undefined :: proxy strategy)

instance Apply Foldable fs => HasDeclaration' (Union fs) Markup.Section where
  toDeclaration' Blob{..} _ (Markup.Section level (Term (In headingAnn headingF), _) _)
    = Just $ SectionDeclaration (maybe (getSource (byteRange headingAnn)) (getSource . sconcat) (nonEmpty (byteRange . termAnnotation . unTerm <$> toList headingF))) level
    where getSource = firstLine . toText . flip Source.slice blobSource
          firstLine = T.takeWhile (/= '\n')

instance HasDeclaration' whole Syntax.Error where
  toDeclaration' Blob{..} ann err@Syntax.Error{}
    = Just $ ErrorDeclaration (T.pack (formatTOCError (Syntax.unError (sourceSpan ann) err))) blobLanguage

instance (Syntax.Empty :< fs) => HasDeclaration' (Union fs) Declaration.Function where
  toDeclaration' Blob{..} _ (Declaration.Function _ (Term (In identifierAnn identifier), _) _ _)
    -- Do not summarize anonymous functions
    | Just Syntax.Empty <- prj identifier = Nothing
    | otherwise                           = Just $ FunctionDeclaration (getSource identifierAnn)
    where getSource = toText . flip Source.slice blobSource . byteRange

instance Apply (HasDeclaration (Union fs)) fs => HasDeclaration' (Union fs) (Union fs) where
  toDeclaration' blob ann = apply (Proxy :: Proxy (HasDeclaration (Union fs))) (toDeclaration blob ann)


data Strategy = Default | Custom

class HasDeclarationWithStrategy (strategy :: Strategy) whole part where
  toDeclarationWithStrategy :: (HasField fields Range, HasField fields Span) => proxy strategy -> Blob -> Record fields -> RAlgebra part (Term whole (Record fields)) (Maybe Declaration)


type family DeclarationStrategy syntax where
  DeclarationStrategy Markup.Section = 'Custom
  DeclarationStrategy a = 'Default


instance HasDeclarationWithStrategy 'Default term syntax where
  toDeclarationWithStrategy _ _ _ _ = Nothing

instance HasDeclaration' term syntax => HasDeclarationWithStrategy 'Custom term syntax where
  toDeclarationWithStrategy _ = toDeclaration'


getDeclaration :: HasField fields (Maybe Declaration) => Record fields -> Maybe Declaration
getDeclaration = getField

-- | Produce the annotations of nodes representing declarations.
declaration :: HasField fields (Maybe Declaration) => TermF f (Record fields) a -> Maybe (Record fields)
declaration (In annotation _) = annotation <$ (getField annotation :: Maybe Declaration)


-- | Compute 'Declaration's for methods and functions in 'Syntax'.
syntaxDeclarationAlgebra :: HasField fields Range => Blob -> RAlgebra (TermF S.Syntax (Record fields)) (Term S.Syntax (Record fields)) (Maybe Declaration)
syntaxDeclarationAlgebra Blob{..} (In a r) = case r of
  S.Function (identifier, _) _ _ -> Just $ FunctionDeclaration (getSource identifier)
  S.Method _ (identifier, _) Nothing _ _ -> Just $ MethodDeclaration (getSource identifier)
  S.Method _ (identifier, _) (Just (receiver, _)) _ _
    | S.Indexed [receiverParams] <- unwrap receiver
    , S.ParameterDecl (Just ty) _ <- unwrap receiverParams -> Just $ MethodDeclaration ("(" <> getSource ty <> ") " <> getSource identifier)
    | otherwise -> Just $ MethodDeclaration (getSource receiver <> "." <> getSource identifier)
  S.ParseError{} -> Just $ ErrorDeclaration (toText (Source.slice (byteRange a) blobSource)) blobLanguage
  _ -> Nothing
  where getSource = toText . flip Source.slice blobSource . byteRange . extract

-- | Compute 'Declaration's for methods and functions.
declarationAlgebra :: (Declaration.Function :< fs, Declaration.Method :< fs, Syntax.Empty :< fs, Syntax.Error :< fs, Apply Functor fs, HasField fields Range, HasField fields Span)
                   => Blob
                   -> RAlgebra (TermF (Union fs) (Record fields)) (Term (Union fs) (Record fields)) (Maybe Declaration)
declarationAlgebra Blob{..} (In a r)
  -- Do not summarize anonymous functions
  | Just (Declaration.Function _ (identifier, _) _ _) <- prj r
  , Just Syntax.Empty <- prj (unwrap identifier)
  = Nothing

  -- Named functions
  | Just (Declaration.Function _ (identifier, _) _ _) <- prj r
  = Just $ FunctionDeclaration (getSource (extract identifier))

  -- Methods without a receiver
  | Just (Declaration.Method _ (receiver, _) (identifier, _) _ _) <- prj r
  , Just Syntax.Empty <- prj (unwrap receiver)
  = Just $ MethodDeclaration (getSource (extract identifier))

  -- Methods with a receiver (class methods) are formatted like `receiver.method_name`
  | Just (Declaration.Method _ (receiver, _) (identifier, _) _ _) <- prj r
  = Just $ MethodDeclaration (getSource (extract receiver) <> "." <> getSource (extract identifier))

  | Just err@Syntax.Error{} <- prj r
  = Just $ ErrorDeclaration (T.pack (formatTOCError (Syntax.unError (sourceSpan a) err))) blobLanguage
  | otherwise = Nothing

  where getSource = toText . flip Source.slice blobSource . byteRange

-- | Compute 'Declaration's with the headings of 'Markup.Section's.
markupSectionAlgebra :: (Markup.Section :< fs, Syntax.Error :< fs, HasField fields Range, HasField fields Span, Apply Functor fs, Apply Foldable fs)
                     => Blob
                     -> RAlgebra (TermF (Union fs) (Record fields)) (Term (Union fs) (Record fields)) (Maybe Declaration)
markupSectionAlgebra Blob{..} (In a r)
  | Just (Markup.Section level (heading, _) _) <- prj r = Just $ SectionDeclaration (maybe (getSource (extract heading)) (firstLine . toText . flip Source.slice blobSource . sconcat) (nonEmpty (byteRange . extract <$> toList (unwrap heading)))) level
  | Just err@Syntax.Error{} <- prj r = Just $ ErrorDeclaration (T.pack (formatTOCError (Syntax.unError (sourceSpan a) err))) blobLanguage
  | otherwise = Nothing
  where getSource = firstLine . toText . flip Source.slice blobSource . byteRange
        firstLine = T.takeWhile (/= '\n')

formatTOCError :: Error.Error String -> String
formatTOCError e = showExpectation False (errorExpected e) (errorActual e) ""

-- | An entry in a table of contents.
data Entry a
  = Unchanged { entryPayload :: a } -- ^ An entry for an unchanged portion of a diff (i.e. a diff node not containing any patches).
  | Changed   { entryPayload :: a } -- ^ An entry for a node containing changes.
  | Inserted  { entryPayload :: a } -- ^ An entry for a change occurring inside an 'Insert' 'Patch'.
  | Deleted   { entryPayload :: a } -- ^ An entry for a change occurring inside a 'Delete' 'Patch'.
  | Replaced  { entryPayload :: a } -- ^ An entry for a change occurring on the insertion side of a 'Replace' 'Patch'.
  deriving (Eq, Show)


-- | Compute a table of contents for a diff characterized by a function mapping relevant nodes onto values in Maybe.
tableOfContentsBy :: (Foldable f, Functor f)
                  => (forall b. TermF f ann b -> Maybe a) -- ^ A function mapping relevant nodes onto values in Maybe.
                  -> Diff f ann ann                       -- ^ The diff to compute the table of contents for.
                  -> [Entry a]                            -- ^ A list of entries for relevant changed and unchanged nodes in the diff.
tableOfContentsBy selector = fromMaybe [] . cata (\ r -> case r of
  Patch patch -> (pure . patchEntry <$> bicrosswalk selector selector patch) <> bifoldMap fold fold patch <> Just []
  Merge (In (_, ann2) r) -> case (selector (In ann2 r), fold r) of
    (Just a, Nothing) -> Just [Unchanged a]
    (Just a, Just []) -> Just [Changed a]
    (_     , entries) -> entries)

  where patchEntry = patch Deleted Inserted (const Replaced)

termTableOfContentsBy :: (Foldable f, Functor f)
                      => (forall b. TermF f annotation b -> Maybe a)
                      -> Term f annotation
                      -> [a]
termTableOfContentsBy selector = cata termAlgebra
  where termAlgebra r | Just a <- selector r = [a]
                      | otherwise = fold r


newtype DedupeKey = DedupeKey (Maybe T.Text, Maybe T.Text) deriving (Eq, Ord)

-- Dedupe entries in a final pass. This catches two specific scenarios with
-- different behaviors:
-- 1. Identical entries are in the list.
--    Action: take the first one, drop all subsequent.
-- 2. Two similar entries (defined by a case insensitive comparision of their
--    identifiers) are in the list.
--    Action: Combine them into a single Replaced entry.
dedupe :: forall fields. HasField fields (Maybe Declaration) => [Entry (Record fields)] -> [Entry (Record fields)]
dedupe = let tuples = sortOn fst . Map.elems . snd . foldl' go (0, Map.empty) in (fmap . fmap) snd tuples
  where
    go :: HasField fields (Maybe Declaration)
       => (Int, Map.Map DedupeKey (Int, Entry (Record fields)))
       -> Entry (Record fields)
       -> (Int, Map.Map DedupeKey (Int, Entry (Record fields)))
    go (index, m) x | Just (_, similar) <- Map.lookup (dedupeKey x) m
                    = if exactMatch similar x
                      then (succ index, m)
                      else
                        let replacement = Replaced (entryPayload similar)
                        in (succ index, Map.insert (dedupeKey replacement) (index, replacement) m)
                    | otherwise = (succ index, Map.insert (dedupeKey x) (index, x) m)

    dedupeKey entry = DedupeKey ((fmap toCategoryName . getDeclaration . entryPayload) entry, (fmap (toLower . declarationIdentifier) . getDeclaration . entryPayload) entry)
    exactMatch = (==) `on` (getDeclaration . entryPayload)

-- | Construct a 'JSONSummary' from an 'Entry'. Returns 'Nothing' for 'Unchanged' patches.
entrySummary :: (HasField fields (Maybe Declaration), HasField fields Span) => Entry (Record fields) -> Maybe JSONSummary
entrySummary entry = case entry of
  Unchanged _ -> Nothing
  Changed a   -> recordSummary a "modified"
  Deleted a   -> recordSummary a "removed"
  Inserted a  -> recordSummary a "added"
  Replaced a  -> recordSummary a "modified"

-- | Construct a 'JSONSummary' from a node annotation and a change type label.
recordSummary :: (HasField fields (Maybe Declaration), HasField fields Span) => Record fields -> T.Text -> Maybe JSONSummary
recordSummary record = case getDeclaration record of
  Just (ErrorDeclaration text language) -> Just . const (ErrorSummary text (sourceSpan record) language)
  Just declaration -> Just . JSONSummary (toCategoryName declaration) (declarationIdentifier declaration) (sourceSpan record)
  Nothing -> const Nothing

renderToCDiff :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => Both Blob -> Diff f (Record fields) (Record fields) -> Summaries
renderToCDiff blobs = uncurry Summaries . bimap toMap toMap . List.partition isValidSummary . diffTOC
  where toMap [] = mempty
        toMap as = Map.singleton summaryKey (toJSON <$> as)
        summaryKey = T.pack $ case runJoin (blobPath <$> blobs) of
          (before, after) | null before -> after
                          | null after -> before
                          | before == after -> after
                          | otherwise -> before <> " -> " <> after

renderToCTerm :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => Blob -> Term f (Record fields) -> Summaries
renderToCTerm Blob{..} = uncurry Summaries . bimap toMap toMap . List.partition isValidSummary . termToC
  where toMap [] = mempty
        toMap as = Map.singleton (T.pack blobPath) (toJSON <$> as)

diffTOC :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => Diff f (Record fields) (Record fields) -> [JSONSummary]
diffTOC = mapMaybe entrySummary . dedupe . tableOfContentsBy declaration

termToC :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => Term f (Record fields) -> [JSONSummary]
termToC = mapMaybe (flip recordSummary "unchanged") . termTableOfContentsBy declaration

-- The user-facing category name
toCategoryName :: Declaration -> T.Text
toCategoryName declaration = case declaration of
  FunctionDeclaration _ -> "Function"
  MethodDeclaration _ -> "Method"
  SectionDeclaration _ l -> "Heading " <> T.pack (show l)
  ErrorDeclaration{} -> "ParseError"
