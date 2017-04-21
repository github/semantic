{-# LANGUAGE GADTs, MultiParamTypeClasses  #-}
module Renderer
( DiffRenderer(..)
, runDiffRenderer
, ParseTreeRenderer(..)
, runParseTreeRenderer
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
import Term

import Control.Parallel.Strategies
import qualified Control.Concurrent.Async as Async

data DiffRenderer fields output where
  SplitRenderer :: (HasField fields Category, HasField fields Range) => DiffRenderer fields File
  PatchRenderer :: HasField fields Range => DiffRenderer fields File
  JSONDiffRenderer :: (ToJSON (Record fields), HasField fields Category, HasField fields Range) => DiffRenderer fields (Map Text Value)
  SummaryRenderer :: HasDefaultFields fields => DiffRenderer fields Summaries
  SExpressionDiffRenderer :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> DiffRenderer fields ByteString
  ToCRenderer :: HasDefaultFields fields => DiffRenderer fields Summaries

runDiffRenderer :: (Monoid output, StringConv output ByteString) => DiffRenderer fields output -> [(Both SourceBlob, Diff (Syntax Text) (Record fields))] -> IO output
runDiffRenderer renderer = renderAsync $ case renderer of
  SplitRenderer -> (File .) . R.split
  PatchRenderer -> (File .) . R.patch
  JSONDiffRenderer -> R.json
  SummaryRenderer -> R.summary
  SExpressionDiffRenderer format -> R.sExpression format
  ToCRenderer -> R.toc

  where
    renderAsync :: (Monoid output, StringConv output ByteString) => (Both SourceBlob -> Diff (Syntax Text) (Record fields) -> output) -> [(Both SourceBlob, Diff (Syntax Text) (Record fields))] -> IO output
    renderAsync f diffs = do
      outputs <- Async.mapConcurrently (pure . uncurry f) diffs
      pure $ mconcat (outputs `using` parTraversable rseq)


data ParseTreeRenderer fields output where
  SExpressionParseTreeRenderer :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> ParseTreeRenderer fields ByteString
  JSONParseTreeRenderer :: HasDefaultFields fields => ParseTreeRenderer fields Value
  JSONIndexParseTreeRenderer :: HasDefaultFields fields => ParseTreeRenderer fields Value

runParseTreeRenderer :: (Monoid output, StringConv output ByteString) => ParseTreeRenderer fields output -> [(SourceBlob, Term (Syntax Text) (Record fields))] -> IO output
runParseTreeRenderer renderer = renderAsync $ case renderer of
  SExpressionParseTreeRenderer format -> R.sExpressionParseTree format
  JSONParseTreeRenderer -> R.jsonParseTree False
  JSONIndexParseTreeRenderer -> R.jsonIndexParseTree False

  where
    renderAsync :: (Monoid output, StringConv output ByteString) => (SourceBlob -> Term (Syntax Text) (Record fields) -> output) -> [(SourceBlob, Term (Syntax Text) (Record fields))] -> IO output
    renderAsync f terms = do
      outputs <- Async.mapConcurrently (pure . uncurry f) terms
      pure $ mconcat (outputs `using` parTraversable rseq)

newtype File = File { unFile :: Text }
  deriving Show

instance StringConv File ByteString where
  strConv _ = encodeUtf8 . unFile

instance Show (DiffRenderer fields output) where
  showsPrec _ SplitRenderer = showString "SplitRenderer"
  showsPrec _ PatchRenderer = showString "PatchRenderer"
  showsPrec _ JSONDiffRenderer = showString "JSONDiffRenderer"
  showsPrec _ SummaryRenderer = showString "SummaryRenderer"
  showsPrec d (SExpressionDiffRenderer format) = showsUnaryWith showsPrec "SExpressionDiffRenderer" d format
  showsPrec _ ToCRenderer = showString "ToCRenderer"

instance Show (ParseTreeRenderer fields output) where
  showsPrec d (SExpressionParseTreeRenderer format) = showsUnaryWith showsPrec "SExpressionParseTreeRenderer" d format
  showsPrec _ JSONParseTreeRenderer = showString "JSONParseTreeRenderer"
  showsPrec _ JSONIndexParseTreeRenderer = showString "JSONIndexParseTreeRenderer"

instance Monoid File where
  mempty = File mempty
  mappend (File a) (File b) = File (a <> "\n" <> b)
