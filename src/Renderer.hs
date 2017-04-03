{-# LANGUAGE GADTs #-}
module Renderer
( DiffRenderer(..)
, runDiffRenderer
, Format(..)
, Summaries(..)
, File(..)
) where

import Data.Aeson (ToJSON, Value)
import Data.Functor.Both
import Data.Map as Map hiding (null)
import Data.Functor.Listable
import Data.Record
import Diff
import Info
import Prologue
import Renderer.JSON as R
import Renderer.Patch as R
import Renderer.SExpression as R
import Renderer.Split as R
import Renderer.Summary as R
import Renderer.TOC as R
import Source (SourceBlob)
import Syntax

data DiffRenderer fields output where
  SplitRenderer :: (HasField fields Category, HasField fields Range) => DiffRenderer fields File
  PatchRenderer :: HasField fields Range => DiffRenderer fields File
  JSONDiffRenderer :: (ToJSON (Record fields), HasField fields Category, HasField fields Range) => DiffRenderer fields (Map Text Value)
  SummaryRenderer :: HasDefaultFields fields => DiffRenderer fields Summaries
  SExpressionDiffRenderer :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> DiffRenderer fields ByteString
  ToCRenderer :: HasDefaultFields fields => DiffRenderer fields Summaries

runDiffRenderer :: Monoid output => DiffRenderer fields output -> [(Both SourceBlob, Diff (Syntax Text) (Record fields))] -> output
runDiffRenderer renderer = foldMap . uncurry $ case renderer of
  SplitRenderer -> (File .) . R.split
  PatchRenderer -> (File .) . R.patch
  JSONDiffRenderer -> R.json
  SummaryRenderer -> R.summary
  SExpressionDiffRenderer format -> R.sExpression format
  ToCRenderer -> R.toc

-- | The available types of diff rendering.
data Format = Split | Patch | JSON | Summary | SExpression | TOC | Index | ParseTree
  deriving (Show)

newtype File = File { unFile :: Text }
  deriving Show

instance Monoid File where
  mempty = File mempty
  mappend (File a) (File b) = File (a <> "\n" <> b)

instance Listable Format where
  tiers = cons0 Split
       \/ cons0 Patch
       \/ cons0 JSON
       \/ cons0 Summary
       \/ cons0 SExpression
       \/ cons0 TOC
