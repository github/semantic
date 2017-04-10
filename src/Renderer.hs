{-# LANGUAGE GADTs #-}
module Renderer
( DiffRenderer(..)
, runDiffRenderer
, Summaries(..)
, File(..)
) where

import Data.Aeson (ToJSON, Value)
import Data.Functor.Both
import Data.Functor.Classes
import Text.Show
import Data.Map as Map hiding (null)
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

newtype File = File { unFile :: Text }
  deriving Show

instance Show (DiffRenderer fields output) where
  showsPrec _ SplitRenderer = showString "SplitRenderer"
  showsPrec _ PatchRenderer = showString "PatchRenderer"
  showsPrec _ JSONDiffRenderer = showString "JSONDiffRenderer"
  showsPrec _ SummaryRenderer = showString "SummaryRenderer"
  showsPrec d (SExpressionDiffRenderer format) = showsUnaryWith showsPrec "SExpressionDiffRenderer" d format
  showsPrec _ ToCRenderer = showString "ToCRenderer"

instance Monoid File where
  mempty = File mempty
  mappend (File a) (File b) = File (a <> "\n" <> b)
