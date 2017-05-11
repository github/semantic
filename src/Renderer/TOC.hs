{-# LANGUAGE DeriveAnyClass, RankNTypes #-}
module Renderer.TOC
( toc
, diffTOC
, JSONSummary(..)
, Summarizable(..)
, isValidSummary
, Declaration(..)
, declaration
, declarationAlgebra
, Entry(..)
, entryPayload
, tableOfContentsBy
, dedupe
, entrySummary
) where

import Category as C
import Data.Aeson
import Data.Align (crosswalk)
import Data.Functor.Both hiding (fst, snd)
import qualified Data.Functor.Both as Both
import Data.Functor.Listable
import Data.Text (toLower)
import Data.Text.Listable
import Data.These
import Data.Record
import Diff
import Info
import Patch
import Prologue
import Renderer.Summary (Summaries(..))
import qualified Data.List as List
import qualified Data.Map as Map hiding (null)
import Source hiding (null)
import Syntax as S
import Term

data JSONSummary = JSONSummary { info :: Summarizable }
                 | ErrorSummary { error :: Text, errorSpan :: SourceSpan }
                 deriving (Generic, Eq, Show)

instance ToJSON JSONSummary where
  toJSON (JSONSummary Summarizable{..}) = object [ "changeType" .= summarizableChangeType, "category" .= toCategoryName summarizableCategory, "term" .= summarizableTermName, "span" .= summarizableSourceSpan ]
  toJSON ErrorSummary{..} = object [ "error" .= error, "span" .= errorSpan ]

isValidSummary :: JSONSummary -> Bool
isValidSummary ErrorSummary{} = False
isValidSummary _ = True

data Summarizable
  = Summarizable
    { summarizableCategory :: Category
    , summarizableTermName :: Text
    , summarizableSourceSpan :: SourceSpan
    , summarizableChangeType :: Text
    }
  deriving (Eq, Show)

-- | A declaration’s identifier and type.
data Declaration
  = MethodDeclaration   { declarationIdentifier :: Text }
  | FunctionDeclaration { declarationIdentifier :: Text }
  | ErrorDeclaration    { declarationIdentifier :: Text }
  deriving (Eq, Generic, NFData, Show)

-- | Produce the annotations of nodes representing declarations.
declaration :: (HasField fields (Maybe Declaration), HasField fields Category) => TermF (Syntax Text) (Record fields) a -> Maybe (Record fields)
declaration (annotation :< syntax)
  | S.ParseError{} <- syntax = Just (setCategory annotation C.ParseError)
  | otherwise                = annotation <$ (getField annotation :: Maybe Declaration)


-- | Compute 'Declaration's for methods and functions.
declarationAlgebra :: HasField fields Range => Source -> TermF (Syntax Text) (Record fields) (Term (Syntax Text) (Record fields), Maybe Declaration) -> Maybe Declaration
declarationAlgebra source r = case tailF r of
  S.Function (identifier, _) _ _ -> Just $ FunctionDeclaration (getSource identifier)
  S.Method _ (identifier, _) Nothing _ _ -> Just $ MethodDeclaration (getSource identifier)
  S.Method _ (identifier, _) (Just (receiver, _)) _ _
    | S.Indexed [receiverParams] <- unwrap receiver
    , S.ParameterDecl (Just ty) _ <- unwrap receiverParams -> Just $ MethodDeclaration ("(" <> getSource ty <> ") " <> getSource identifier)
    | otherwise -> Just $ MethodDeclaration (getSource receiver <> "." <> getSource identifier)
  S.ParseError{} -> Just $ ErrorDeclaration (toText (Source.slice (byteRange (headF r)) source))
  _ -> Nothing
  where getSource = toText . flip Source.slice source . byteRange . extract


-- | An entry in a table of contents.
data Entry a
  = Unchanged a -- ^ An entry for an unchanged portion of a diff (i.e. a diff node not containing any patches).
  | Changed a   -- ^ An entry for a node containing changes.
  | Inserted a  -- ^ An entry for a change occurring inside an 'Insert' 'Patch'.
  | Deleted a   -- ^ An entry for a change occurring inside a 'Delete' 'Patch'.
  | Replaced a  -- ^ An entry for a change occurring on the insertion side of a 'Replace' 'Patch'.
  deriving (Eq, Show)

entryPayload :: Entry a -> a
entryPayload (Unchanged a) = a
entryPayload (Changed a) = a
entryPayload (Inserted a) = a
entryPayload (Deleted a) = a
entryPayload (Replaced a) = a


-- | Compute a table of contents for a diff characterized by a function mapping relevant nodes onto values in Maybe.
tableOfContentsBy :: Traversable f
                  => (forall b. TermF f annotation b -> Maybe a) -- ^ A function mapping relevant nodes onto values in Maybe.
                  -> Diff f annotation                           -- ^ The diff to compute the table of contents for.
                  -> [Entry a]                                   -- ^ A list of entries for relevant changed and unchanged nodes in the diff.
tableOfContentsBy selector = fromMaybe [] . iter diffAlgebra . fmap (Just . fmap patchEntry . crosswalk (cata termAlgebra))
  where diffAlgebra r = case (selector (first Both.snd r), fold r) of
          (Just a, Nothing) -> Just [Unchanged a]
          (Just a, Just []) -> Just [Changed a]
          (_     , entries) -> entries
        termAlgebra r | Just a <- selector r = [a]
                      | otherwise = fold r
        patchEntry = these Deleted Inserted (const Replaced) . unPatch

dedupe :: (HasField fields Category, HasField fields (Maybe Declaration)) => [Entry (Record fields)] -> [Entry (Record fields)]
dedupe = foldl' go []
  where go xs x | (_, _:_) <- find (exactMatch `on` entryPayload) x xs = xs
                | (front, similar : back) <- find (similarMatch `on` entryPayload) x xs =
                  front <> (Replaced (entryPayload similar) : back)
                | otherwise = xs <> [x]

        find p x = List.break (p x)
        exactMatch = (==) `on` getDeclaration
        similarMatch a b = sameCategory a b && similarDeclaration a b
        sameCategory = (==) `on` category
        similarDeclaration = (==) `on` fmap (toLower . declarationIdentifier) . getDeclaration
        getDeclaration :: HasField fields (Maybe Declaration) => Record fields -> Maybe Declaration
        getDeclaration = getField

-- | Construct a 'JSONSummary' from an 'Entry'. Returns 'Nothing' for 'Unchanged' patches.
entrySummary :: (HasField fields Category, HasField fields (Maybe Declaration), HasField fields SourceSpan) => Entry (Record fields) -> Maybe JSONSummary
entrySummary entry = case entry of
  Unchanged _ -> Nothing
  Changed a   -> Just (recordSummary a "modified")
  Deleted a   -> Just (recordSummary a "removed")
  Inserted a  -> Just (recordSummary a "added")
  Replaced a  -> Just (recordSummary a "modified")
  where recordSummary record
          | C.ParseError <- category record = const (ErrorSummary (maybe "" declarationIdentifier (getField record :: Maybe Declaration)) (sourceSpan record))
          | otherwise = JSONSummary . Summarizable (category record) (maybe "" declarationIdentifier (getField record :: Maybe Declaration)) (sourceSpan record)

toc :: (HasField fields Category, HasField fields (Maybe Declaration), HasField fields SourceSpan) => Both SourceBlob -> Diff (Syntax Text) (Record fields) -> Summaries
toc blobs = uncurry Summaries . bimap toMap toMap . List.partition isValidSummary . diffTOC
  where toMap [] = mempty
        toMap as = Map.singleton summaryKey (toJSON <$> as)
        summaryKey = toS $ case runJoin (path <$> blobs) of
          (before, after) | null before -> after
                          | null after -> before
                          | before == after -> after
                          | otherwise -> before <> " -> " <> after

diffTOC :: (HasField fields Category, HasField fields (Maybe Declaration), HasField fields SourceSpan) => Diff (Syntax Text) (Record fields) -> [JSONSummary]
diffTOC = mapMaybe entrySummary . dedupe . tableOfContentsBy declaration

-- The user-facing category name
toCategoryName :: Category -> Text
toCategoryName category = case category of
  C.SingletonMethod -> "Method"
  c -> show c

instance Listable Declaration where
  tiers
    =  cons1 (MethodDeclaration . unListableText)
    \/ cons1 (FunctionDeclaration . unListableText)
    \/ cons1 (ErrorDeclaration . unListableText)
