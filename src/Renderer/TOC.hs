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
, HasDeclaration
, declarationAlgebra
, syntaxDeclarationAlgebra
, Entry(..)
, tableOfContentsBy
, dedupe
, entrySummary
) where

import Data.Aeson
import Data.Align (align, bicrosswalk)
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
import Data.Range
import Data.Record
import Data.Semigroup ((<>), sconcat)
import Data.Source as Source
import Data.Term
import Data.Text (toLower)
import qualified Data.Text as T
import Data.These
import Data.Union
import GHC.Generics
import Info
import Language
import qualified Data.List as List
import qualified Data.Map as Map hiding (null)
import Syntax as S
import Data.Syntax.Algebra (CyclomaticComplexity(..), RAlgebra)
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import qualified Language.Markdown.Syntax as Markdown

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


-- | An r-algebra producing 'Just' a 'Declaration' for syntax nodes corresponding to high-level declarations, or 'Nothing' otherwise.
--
--   Customizing this for a given syntax type involves two steps:
--
--   1. Defining a 'CustomHasDeclaration' instance for the type.
--   2. Adding the type to the 'DeclarationStrategy' type family.
--
--   If you’re getting errors about missing a 'CustomHasDeclaration' instance for your syntax type, you probably forgot step 1.
--
--   If you’re getting 'Nothing' for your syntax node at runtime, you probably forgot step 2.
declarationAlgebra :: (HasField fields Range, HasField fields Span, Foldable syntax, HasDeclaration syntax) => Blob -> RAlgebra (TermF syntax (Record fields)) (Term syntax (Record fields)) (Maybe Declaration)
declarationAlgebra blob (In ann syntax) = toDeclaration blob ann syntax


-- | Types for which we can produce a 'Declaration' in 'Maybe'. There is exactly one instance of this typeclass; adding customized 'Declaration's for a new type is done by defining an instance of 'CustomHasDeclaration' instead.
--
--   This typeclass employs the Advanced Overlap techniques designed by Oleg Kiselyov & Simon Peyton Jones: https://wiki.haskell.org/GHC/AdvancedOverlap.
class HasDeclaration syntax where
  -- | Compute a 'Declaration' for a syntax type using its 'CustomHasDeclaration' instance, if any, or else falling back to the default definition (which simply returns 'Nothing').
  toDeclaration :: (Foldable whole, HasField fields Range, HasField fields Span) => Blob -> Record fields -> RAlgebra syntax (Term whole (Record fields)) (Maybe Declaration)

-- | Define 'toDeclaration' using the 'CustomHasDeclaration' instance for a type if there is one or else use the default definition.
--
--   This instance determines whether or not there is an instance for @syntax@ by looking it up in the 'DeclarationStrategy' type family. Thus producing a 'Declaration' for a node requires both defining a 'CustomHasDeclaration' instance _and_ adding a definition for the type to the 'DeclarationStrategy' type family to return 'Custom'.
--
--   Note that since 'DeclarationStrategy' has a fallback case for its final entry, this instance will hold for all types of kind @* -> *@. Thus, this must be the only instance of 'HasDeclaration', as any other instance would be indistinguishable.
instance (DeclarationStrategy syntax ~ strategy, HasDeclarationWithStrategy strategy syntax) => HasDeclaration syntax where
  toDeclaration = toDeclarationWithStrategy (Proxy :: Proxy strategy)


-- | Types for which we can produce a customized 'Declaration'. This returns in 'Maybe' so that some values can be opted out (e.g. anonymous functions).
class CustomHasDeclaration syntax where
  -- | Produce a customized 'Declaration' for a given syntax node.
  customToDeclaration :: (Foldable whole, HasField fields Range, HasField fields Span) => Blob -> Record fields -> RAlgebra syntax (Term whole (Record fields)) (Maybe Declaration)


-- | Produce a 'SectionDeclaration' from the first line of the heading of a 'Markdown.Section' node.
instance CustomHasDeclaration Markdown.Section where
  customToDeclaration Blob{..} _ (Markdown.Section level (Term (In headingAnn headingF), _) _)
    = Just $ SectionDeclaration (maybe (getSource (byteRange headingAnn)) (getSource . sconcat) (nonEmpty (byteRange . termAnnotation . unTerm <$> toList headingF))) level
    where getSource = firstLine . toText . flip Source.slice blobSource
          firstLine = T.takeWhile (/= '\n')

-- | Produce an 'ErrorDeclaration' for 'Syntax.Error' nodes.
instance CustomHasDeclaration Syntax.Error where
  customToDeclaration Blob{..} ann err@Syntax.Error{}
    = Just $ ErrorDeclaration (T.pack (formatTOCError (Syntax.unError (sourceSpan ann) err))) blobLanguage

-- | Produce a 'FunctionDeclaration' for 'Declaration.Function' nodes so long as their identifier is non-empty (defined as having a non-empty 'byteRange').
instance CustomHasDeclaration Declaration.Function where
  customToDeclaration Blob{..} _ (Declaration.Function _ (Term (In identifierAnn _), _) _ _)
    -- Do not summarize anonymous functions
    | isEmpty identifierAnn = Nothing
    -- Named functions
    | otherwise             = Just $ FunctionDeclaration (getSource identifierAnn)
    where getSource = toText . flip Source.slice blobSource . byteRange
          isEmpty = (== 0) . rangeLength . byteRange

-- | Produce a 'MethodDeclaration' for 'Declaration.Method' nodes. If the method’s receiver is non-empty (defined as having a non-empty 'byteRange'), the 'declarationIdentifier' will be formatted as 'receiver.method_name'; otherwise it will be simply 'method_name'.
instance CustomHasDeclaration Declaration.Method where
  customToDeclaration Blob{..} _ (Declaration.Method _ (Term (In receiverAnn _), _) (Term (In identifierAnn _), _) _ _)
    -- Methods without a receiver
    | isEmpty receiverAnn = Just $ MethodDeclaration (getSource identifierAnn)
    -- Methods with a receiver (class methods) are formatted like `receiver.method_name`
    | otherwise           = Just $ MethodDeclaration (getSource receiverAnn <> "." <> getSource identifierAnn)
    where getSource = toText . flip Source.slice blobSource . byteRange
          isEmpty = (== 0) . rangeLength . byteRange

-- | Produce a 'Declaration' for 'Union's using the 'HasDeclaration' instance & therefore using a 'CustomHasDeclaration' instance when one exists & the type is listed in 'DeclarationStrategy'.
instance Apply HasDeclaration fs => CustomHasDeclaration (Union fs) where
  customToDeclaration blob ann = apply (Proxy :: Proxy HasDeclaration) (toDeclaration blob ann)


-- | A strategy for defining a 'HasDeclaration' instance. Intended to be promoted to the kind level using @-XDataKinds@.
data Strategy = Default | Custom

-- | Produce a 'Declaration' for a syntax node using either the 'Default' or 'Custom' strategy.
--
--   You should probably be using 'CustomHasDeclaration' instead of this class; and you should not define new instances of this class.
class HasDeclarationWithStrategy (strategy :: Strategy) syntax where
  toDeclarationWithStrategy :: (Foldable whole, HasField fields Range, HasField fields Span) => proxy strategy -> Blob -> Record fields -> RAlgebra syntax (Term whole (Record fields)) (Maybe Declaration)


-- | A predicate on syntax types selecting either the 'Custom' or 'Default' strategy.
--
--   Only entries for which we want to use the 'Custom' strategy should be listed, with the exception of the final entry which maps all other types onto the 'Default' strategy.
--
--   If you’re seeing errors about missing a 'CustomHasDeclaration' instance for a given type, you’ve probably listed it in here but not defined a 'CustomHasDeclaration' instance for it, or else you’ve listed the wrong type in here. Conversely, if your 'customHasDeclaration' method is never being called, you may have forgotten to list the type in here.
type family DeclarationStrategy syntax where
  DeclarationStrategy Declaration.Function = 'Custom
  DeclarationStrategy Declaration.Method = 'Custom
  DeclarationStrategy Markdown.Section = 'Custom
  DeclarationStrategy Syntax.Error = 'Custom
  DeclarationStrategy (Union fs) = 'Custom
  DeclarationStrategy a = 'Default


-- | The 'Default' strategy produces 'Nothing'.
instance HasDeclarationWithStrategy 'Default syntax where
  toDeclarationWithStrategy _ _ _ _ = Nothing

-- | The 'Custom' strategy delegates the selection of the strategy to the 'CustomHasDeclaration' instance for the type.
instance CustomHasDeclaration syntax => HasDeclarationWithStrategy 'Custom syntax where
  toDeclarationWithStrategy _ = customToDeclaration


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

formatTOCError :: Error.Error String -> String
formatTOCError e = showExpectation False (errorExpected e) (errorActual e) ""

-- | An entry in a table of contents.
data Entry a
  = Changed (These a a) -- ^ An entry for a node containing changes.
  | Inserted a -- ^ An entry for a change occurring inside an 'Insert' 'Patch'.
  | Deleted  a -- ^ An entry for a change occurring inside a 'Delete' 'Patch'.
  | Replaced a -- ^ An entry for a change occurring on the insertion side of a 'Replace' 'Patch'.
  deriving (Eq, Show)

entryPayload :: Entry a -> a
entryPayload (Changed (These a1 a2)) = a2
entryPayload (Changed (This a1)) = a1
entryPayload (Changed (That a2)) = a2
entryPayload (Inserted a2) = a2
entryPayload (Deleted a1) = a1
entryPayload (Replaced a2) = a2

-- | Compute a table of contents for a diff characterized by a function mapping relevant nodes onto values in Maybe.
tableOfContentsBy :: (Foldable f, Functor f)
                  => (forall b. TermF f ann b -> Maybe a) -- ^ A function mapping relevant nodes onto values in Maybe.
                  -> Diff f ann ann                       -- ^ The diff to compute the table of contents for.
                  -> [Entry a]                            -- ^ A list of entries for relevant changed nodes in the diff.
tableOfContentsBy selector = fromMaybe [] . cata (\ r -> case r of
  Patch patch -> (pure . patchEntry <$> bicrosswalk selector selector patch) <> bifoldMap fold fold patch <> Just []
  Merge (In (ann1, ann2) r) -> case (selector (In ann1 r) `align` selector (In ann2 r), fold r) of
    (Just a, Just entries) -> Just (Changed a : entries)
    (_     , entries)      -> entries)

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

-- | Construct a 'JSONSummary' from an 'Entry'.
entrySummary :: (HasField fields (Maybe Declaration), HasField fields Span, HasField fields CyclomaticComplexity) => Entry (Record fields) -> Maybe JSONSummary
entrySummary entry = case entry of
  Changed a   -> recordSummary a "modified" (Just (getField a))
  Deleted a   -> recordSummary a "removed" (Just (getField a))
  Inserted a  -> recordSummary a "added" (Just (getField a))
  Replaced a  -> recordSummary a "modified" (Just (getField a))


-- | Construct a 'JSONSummary' from a node annotation and a change type label.
recordSummary :: (HasField fields (Maybe Declaration), HasField fields Span, HasField fields CyclomaticComplexity) => Record fields -> T.Text -> Maybe CyclomaticComplexity -> Maybe JSONSummary
recordSummary record entryText complexity = case getDeclaration record of
  Just (ErrorDeclaration text language) -> Just $ ErrorSummary text (sourceSpan record) language
  Just declaration -> Just $ JSONSummary (toCategoryName declaration) (declarationIdentifier declaration <> " " <> (maybe T.empty (\n -> T.pack (show n)) complexity)) (sourceSpan record) entryText
  Nothing -> Nothing

renderToCDiff :: (HasField fields (Maybe Declaration), HasField fields Span, HasField fields CyclomaticComplexity, Foldable f, Functor f) => Both Blob -> Diff f (Record fields) (Record fields) -> Summaries
renderToCDiff blobs = uncurry Summaries . bimap toMap toMap . List.partition isValidSummary . diffTOC
  where toMap [] = mempty
        toMap as = Map.singleton summaryKey (toJSON <$> as)
        summaryKey = T.pack $ case runJoin (blobPath <$> blobs) of
          (before, after) | null before -> after
                          | null after -> before
                          | before == after -> after
                          | otherwise -> before <> " -> " <> after

renderToCTerm :: (HasField fields (Maybe Declaration), HasField fields Span, HasField fields CyclomaticComplexity, Foldable f, Functor f) => Blob -> Term f (Record fields) -> Summaries
renderToCTerm Blob{..} = uncurry Summaries . bimap toMap toMap . List.partition isValidSummary . termToC
  where toMap [] = mempty
        toMap as = Map.singleton (T.pack blobPath) (toJSON <$> as)

diffTOC :: (HasField fields (Maybe Declaration), HasField fields Span, HasField fields CyclomaticComplexity, Foldable f, Functor f) => Diff f (Record fields) (Record fields) -> [JSONSummary]
diffTOC = mapMaybe entrySummary . dedupe . tableOfContentsBy declaration

termToC :: (HasField fields (Maybe Declaration), HasField fields Span, HasField fields CyclomaticComplexity, Foldable f, Functor f) => Term f (Record fields) -> [JSONSummary]
termToC = mapMaybe (\a -> recordSummary a "unchanged" Nothing) . termTableOfContentsBy declaration

-- The user-facing category name
toCategoryName :: Declaration -> T.Text
toCategoryName declaration = case declaration of
  FunctionDeclaration _ -> "Function"
  MethodDeclaration _ -> "Method"
  SectionDeclaration _ l -> "Heading " <> T.pack (show l)
  ErrorDeclaration{} -> "ParseError"
