{-# LANGUAGE DeriveGeneric, DerivingVia, DuplicateRecordFields, LambdaCase, OverloadedStrings, RankNTypes, RecordWildCards, ScopedTypeVariables, TupleSections #-}
module Rendering.TOC
( diffTOC
, Summaries(..)
, TOCSummary(..)
, ErrorSummary(..)
, Change(..)
, tableOfContentsBy
, dedupe
, summarizeChange
) where

import Prologue hiding (index)
import Analysis.TOCSummary
import Data.Aeson (ToJSON(..), Value, (.=), object)
import Data.Diff
import Data.Edit
import Data.Language as Language
import Data.List (sortOn)
import qualified Data.Map.Monoidal as Map
import Data.Term
import qualified Data.Text as T
import Source.Loc

data Summaries = Summaries { changes, errors :: Map.Map T.Text [Value] }
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup Summaries
  deriving Monoid    via GenericMonoid    Summaries

instance ToJSON Summaries where
  toJSON Summaries{..} = object [ "changes" .= changes, "errors" .= errors ]

data TOCSummary = TOCSummary
  { kind   :: Kind
  , ident  :: T.Text
  , span   :: Span
  , change :: Change
  }
  deriving (Eq, Show)

data ErrorSummary = ErrorSummary
  { message  :: T.Text
  , span     :: Span
  , language :: Language
  }
  deriving (Eq, Show)

instance ToJSON TOCSummary where
  toJSON TOCSummary{..} = object [ "changeType" .= change, "category" .= formatKind kind, "term" .= ident, "span" .= span ]

instance ToJSON ErrorSummary where
  toJSON ErrorSummary{..} = object [ "error" .= message, "span" .= span, "language" .= language ]

-- | The kind of a ToC change.
data Change
  = Changed
  | Inserted
  | Deleted
  | Replaced
  deriving (Eq, Show)

instance ToJSON Change where
  toJSON = \case
    Changed  -> "modified"
    Deleted  -> "removed"
    Inserted -> "added"
    Replaced -> "modified"


-- | Produce the annotations of nodes representing declarations.
declaration :: TermF f (Maybe Declaration) a -> Maybe Declaration
declaration (In annotation _) = annotation

-- | Compute a table of contents for a diff characterized by a function mapping relevant nodes onto values in Maybe.
tableOfContentsBy :: (Foldable f, Functor f)
                  => (forall b. TermF f ann b -> Maybe a) -- ^ A function mapping relevant nodes onto values in Maybe.
                  -> Diff f ann ann                       -- ^ The diff to compute the table of contents for.
                  -> [(Change, a)]                        -- ^ A list of entries for relevant changed nodes in the diff.
tableOfContentsBy selector = fromMaybe [] . cata (\case
  Patch edit -> (pure . patchEntry <$> select (bimap selector selector edit)) <> bifoldMap fold fold edit <> Just []
  Merge (In (_, ann2) r) -> case (selector (In ann2 r), fold r) of
    (Just a, Just entries) -> Just ((Changed, a) : entries)
    (_     , entries)      -> entries)
  where patchEntry = edit (Deleted,) (Inserted,) (const (Replaced,))

        select = \case
          Delete  a   -> Delete <$> a
          Insert    b -> Insert <$> b
          Compare a b -> liftA2 Compare a b <|> Delete <$> a <|> Insert <$> b


data DedupeKey = DedupeKey !Kind {-# UNPACK #-} !T.Text
  deriving (Eq, Ord)

data Dedupe = Dedupe
  { index  :: {-# UNPACK #-} !Int
  , change :: !Change
  , decl   :: {-# UNPACK #-} !Declaration
  }

-- Dedupe entries in a final pass. This catches two specific scenarios with
-- different behaviors:
-- 1. Identical entries are in the list.
--    Action: take the first one, drop all subsequent.
-- 2. Two similar entries (defined by a case insensitive comparison of their
--    identifiers) are in the list.
--    Action: Combine them into a single Replaced entry.
dedupe :: [(Change, Declaration)] -> [(Change, Declaration)]
dedupe
  = map ((change :: Dedupe -> Change) &&& decl) -- extract the changes and decls
  . sortOn index                                -- after sorting
  . Map.elems                                   -- the elements of the map
  . foldl' go Map.empty                         -- produced by deduping
  . zipWith (uncurry . Dedupe) [0..] where      -- the indexed inputs
  go m d@(Dedupe _ _ decl) = let key = dedupeKey decl in case Map.lookup key m of
    Just (Dedupe _ _ similar)
      | similar == decl -> m
      | otherwise       -> Map.insert key d { change = Replaced, decl = similar } m
    _                   -> Map.insert key d m

  dedupeKey (Declaration kind ident _ _) = DedupeKey kind (T.toLower ident)

-- | Construct a 'TOCSummary' or 'ErrorSummary' from a 'Change' and 'Declaration'.
summarizeChange :: Change -> Declaration -> Either ErrorSummary TOCSummary
summarizeChange change decl@(Declaration kind text srcSpan language)
  | Error <- kind = Left  $ ErrorSummary text srcSpan language
  | otherwise     = Right $ TOCSummary kind (formatIdentifier decl) srcSpan change

diffTOC :: (Foldable f, Functor f) => Diff f (Maybe Declaration) (Maybe Declaration) -> [Either ErrorSummary TOCSummary]
diffTOC = map (uncurry summarizeChange) . dedupe . tableOfContentsBy declaration
