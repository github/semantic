{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses, RankNTypes, TypeOperators #-}
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
, syntaxDeclarationAlgebra
, Entry(..)
, tableOfContentsBy
, dedupe
, entrySummary
) where

import Control.Comonad (extract)
import Control.Comonad.Cofree (unwrap)
import Control.DeepSeq
import Control.Monad.Free (iter)
import Data.Aeson
import Data.Align (crosswalk)
import Data.Bifunctor (bimap, first)
import Data.Blob
import Data.ByteString.Lazy (toStrict)
import Data.Error as Error (Error(..), formatError)
import Data.Foldable (fold, foldl', toList)
import Data.Functor.Both hiding (fst, snd)
import qualified Data.Functor.Both as Both
import Data.Functor.Foldable (cata)
import Data.Functor.Listable
import Data.Function (on)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Output
import Data.Record
import Data.Semigroup ((<>), sconcat)
import Data.Source as Source
import Data.Text (toLower)
import qualified Data.Text as T
import Data.Text.Listable
import Data.These
import Data.Union
import Diff
import GHC.Generics
import Info
import Language
import Patch
import qualified Data.List as List
import qualified Data.Map as Map hiding (null)
import Syntax as S
import Data.Syntax.Algebra (RAlgebra)
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Markup as Markup
import Term

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
  deriving (Eq, Generic, NFData, Show)

getDeclaration :: HasField fields (Maybe Declaration) => Record fields -> Maybe Declaration
getDeclaration = getField

-- | Produce the annotations of nodes representing declarations.
declaration :: HasField fields (Maybe Declaration) => TermF f (Record fields) a -> Maybe (Record fields)
declaration (annotation :< _) = annotation <$ (getField annotation :: Maybe Declaration)


-- | Compute 'Declaration's for methods and functions in 'Syntax'.
syntaxDeclarationAlgebra :: HasField fields Range => Blob -> RAlgebra (SyntaxTermF fields) (SyntaxTerm fields) (Maybe Declaration)
syntaxDeclarationAlgebra Blob{..} (a :< r) = case r of
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
declarationAlgebra :: (Declaration.Function :< fs, Declaration.Method :< fs, Syntax.Error :< fs, Apply1 Functor fs, HasField fields Range, HasField fields Span)
                   => Blob
                   -> RAlgebra (TermF (Union fs) (Record fields)) (Term (Union fs) (Record fields)) (Maybe Declaration)
declarationAlgebra blob@Blob{..} (a :< r)
  | Just (Declaration.Function (identifier, _) _ _) <- prj r = Just $ FunctionDeclaration (getSource (extract identifier))
  | Just (Declaration.Method _ (identifier, _) _ _) <- prj r = Just $ MethodDeclaration (getSource (extract identifier))
  | Just (Syntax.Error errorExpected errorActual _) <- prj r = Just $ ErrorDeclaration (T.pack (formatError False False blob (Error.Error (sourceSpan a) errorExpected errorActual))) blobLanguage
  | otherwise = Nothing
  where getSource = toText . flip Source.slice blobSource . byteRange

-- | Compute 'Declaration's with the headings of 'Markup.Section's.
markupSectionAlgebra :: (Markup.Section :< fs, Syntax.Error :< fs, HasField fields Range, HasField fields Span, Apply1 Functor fs, Apply1 Foldable fs)
                     => Blob
                     -> RAlgebra (TermF (Union fs) (Record fields)) (Term (Union fs) (Record fields)) (Maybe Declaration)
markupSectionAlgebra blob@Blob{..} (a :< r)
  | Just (Markup.Section level (heading, _) _) <- prj r = Just $ SectionDeclaration (maybe (getSource (extract heading)) (firstLine . toText . flip Source.slice blobSource . sconcat) (nonEmpty (byteRange . extract <$> toList (unwrap heading)))) level
  | Just (Syntax.Error errorExpected errorActual _) <- prj r = Just $ ErrorDeclaration (T.pack (formatError False False blob (Error.Error (sourceSpan a) errorExpected errorActual))) blobLanguage
  | otherwise = Nothing
  where getSource = firstLine . toText . flip Source.slice blobSource . byteRange
        firstLine = T.takeWhile (/= '\n')


-- | An entry in a table of contents.
data Entry a
  = Unchanged { entryPayload :: a } -- ^ An entry for an unchanged portion of a diff (i.e. a diff node not containing any patches).
  | Changed   { entryPayload :: a } -- ^ An entry for a node containing changes.
  | Inserted  { entryPayload :: a } -- ^ An entry for a change occurring inside an 'Insert' 'Patch'.
  | Deleted   { entryPayload :: a } -- ^ An entry for a change occurring inside a 'Delete' 'Patch'.
  | Replaced  { entryPayload :: a } -- ^ An entry for a change occurring on the insertion side of a 'Replace' 'Patch'.
  deriving (Eq, Show)


-- | Compute a table of contents for a diff characterized by a function mapping relevant nodes onto values in Maybe.
tableOfContentsBy :: Traversable f
                  => (forall b. TermF f annotation b -> Maybe a) -- ^ A function mapping relevant nodes onto values in Maybe.
                  -> Diff f annotation                           -- ^ The diff to compute the table of contents for.
                  -> [Entry a]                                   -- ^ A list of entries for relevant changed and unchanged nodes in the diff.
tableOfContentsBy selector = fromMaybe [] . iter diffAlgebra . fmap (Just . fmap patchEntry . crosswalk (termTableOfContentsBy selector))
  where diffAlgebra r = case (selector (first Both.snd r), fold r) of
          (Just a, Nothing) -> Just [Unchanged a]
          (Just a, Just []) -> Just [Changed a]
          (_     , entries) -> entries
        patchEntry = these Deleted Inserted (const Replaced) . unPatch

termTableOfContentsBy :: Traversable f
                      => (forall b. TermF f annotation b -> Maybe a)
                      -> Term f annotation
                      -> [a]
termTableOfContentsBy selector = cata termAlgebra
  where termAlgebra r | Just a <- selector r = [a]
                      | otherwise = fold r

dedupe :: HasField fields (Maybe Declaration) => [Entry (Record fields)] -> [Entry (Record fields)]
dedupe = foldl' go []
  where go xs x | (_, _:_) <- find (exactMatch `on` entryPayload) x xs = xs
                | (front, similar : back) <- find (similarMatch `on` entryPayload) x xs =
                  front <> (Replaced (entryPayload similar) : back)
                | otherwise = xs <> [x]

        find p x = List.break (p x)
        exactMatch = (==) `on` getDeclaration
        similarMatch a b = sameCategory a b && similarDeclaration a b
        sameCategory = (==) `on` fmap toCategoryName . getDeclaration
        similarDeclaration = (==) `on` fmap (toLower . declarationIdentifier) . getDeclaration

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

renderToCDiff :: (HasField fields (Maybe Declaration), HasField fields Span, Traversable f) => Both Blob -> Diff f (Record fields) -> Summaries
renderToCDiff blobs = uncurry Summaries . bimap toMap toMap . List.partition isValidSummary . diffTOC
  where toMap [] = mempty
        toMap as = Map.singleton summaryKey (toJSON <$> as)
        summaryKey = T.pack $ case runJoin (blobPath <$> blobs) of
          (before, after) | null before -> after
                          | null after -> before
                          | before == after -> after
                          | otherwise -> before <> " -> " <> after

renderToCTerm :: (HasField fields (Maybe Declaration), HasField fields Span, Traversable f) => Blob -> Term f (Record fields) -> Summaries
renderToCTerm Blob{..} = uncurry Summaries . bimap toMap toMap . List.partition isValidSummary . termToC
  where toMap [] = mempty
        toMap as = Map.singleton (T.pack blobPath) (toJSON <$> as)

diffTOC :: (HasField fields (Maybe Declaration), HasField fields Span, Traversable f) => Diff f (Record fields) -> [JSONSummary]
diffTOC = mapMaybe entrySummary . dedupe . tableOfContentsBy declaration

termToC :: (HasField fields (Maybe Declaration), HasField fields Span, Traversable f) => Term f (Record fields) -> [JSONSummary]
termToC = mapMaybe (flip recordSummary "unchanged") . termTableOfContentsBy declaration

-- The user-facing category name
toCategoryName :: Declaration -> T.Text
toCategoryName declaration = case declaration of
  FunctionDeclaration _ -> "Function"
  MethodDeclaration _ -> "Method"
  SectionDeclaration _ l -> "Heading " <> T.pack (show l)
  ErrorDeclaration{} -> "ParseError"

instance Listable Declaration where
  tiers
    =  cons1 (MethodDeclaration . unListableText)
    \/ cons1 (FunctionDeclaration . unListableText)
    \/ cons1 (flip ErrorDeclaration Nothing . unListableText)
