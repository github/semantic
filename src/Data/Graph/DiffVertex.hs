module Data.Graph.DiffVertex
( DiffVertex(..)
, DiffVertexTerm(..)
, DeletedTerm(..)
, InsertedTerm(..)
, ReplacedTerm(..)
, MergedTerm(..)

-- rexport
, TermVertex(..)
, TermAnnotation(..)
) where

import Data.Graph
import Data.Graph.TermVertex
import Data.Aeson
import Data.JSON.Fields
import qualified Data.Text as T

-- Diffs

data DiffVertex
  = DiffVertex
  { diffVertexId :: Int
  , diffVertexTerm :: DiffVertexTerm
  } deriving (Eq, Ord, Show)

data DiffVertexTerm
  = Deleted DeletedTerm
  | Inserted InsertedTerm
  | Replaced ReplacedTerm
  | Merged MergedTerm
    deriving (Eq, Ord, Show)

data MergedTerm
  = MergedTerm
  { mergedTermName :: String
  , mergedTermBefore :: TermAnnotation
  , mergedTermAfter :: TermAnnotation
  } deriving (Eq, Ord, Show)

data DeletedTerm
  = DeletedTerm
  { deletedTermName :: String
  , deletedTermBefore :: TermAnnotation
  } deriving (Eq, Ord, Show)

data InsertedTerm
  = InsertedTerm
  { insertedTermName :: String
  , insertedTermAfter :: TermAnnotation
  } deriving (Eq, Ord, Show)

data ReplacedTerm
  = ReplacedTerm
  { replacedTermBefore :: DeletedTerm
  , replacedTermAfter :: InsertedTerm
  } deriving (Eq, Ord, Show)

-- Instances

instance ToJSON MergedTerm where
  toJSON MergedTerm{..} = object
    [ "term"   .= mergedTermName
    , "before" .= mergedTermBefore
    , "after"  .= mergedTermAfter
    ]

instance ToJSON DeletedTerm where
  toJSON DeletedTerm{..} = object
    [ "term"   .= deletedTermName
    , "before" .= deletedTermBefore
    ]

instance ToJSON InsertedTerm where
  toJSON InsertedTerm{..} = object
    [ "term"   .= insertedTermName
    , "after"  .= insertedTermAfter
    ]

instance ToJSON ReplacedTerm where
  toJSON (ReplacedTerm DeletedTerm{..} InsertedTerm{..})
    = object [ "before" .= deleted, "after" .= inserted ]
    where deleted  = object $ [ "term" .= deletedTermName  ] <> toJSONFields deletedTermBefore
          inserted = object $ [ "term" .= insertedTermName ] <> toJSONFields insertedTermAfter

instance ToJSON DiffVertex where
  toJSON (DiffVertex i (Deleted t))  = object [ "id" .= T.pack (show i), "deleted"  .= t ]
  toJSON (DiffVertex i (Inserted t)) = object [ "id" .= T.pack (show i), "inserted" .= t ]
  toJSON (DiffVertex i (Replaced t)) = object [ "id" .= T.pack (show i), "replaced" .= t ]
  toJSON (DiffVertex i (Merged t))   = object [ "id" .= T.pack (show i), "merged"   .= t ]

instance VertexTag DiffVertex where uniqueTag = diffVertexId
