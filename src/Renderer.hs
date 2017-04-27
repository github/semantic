{-# LANGUAGE GADTs, MultiParamTypeClasses  #-}
module Renderer
( DiffRenderer(..)
, resolveDiffRenderer
, runDiffRenderer
, ParseTreeRenderer(..)
, resolveParseTreeRenderer
, runParseTreeRenderer
, Summaries(..)
, File(..)
) where

import Data.Aeson (Value)
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


data DiffRenderer fields output where
  SplitRenderer :: (HasField fields Category, HasField fields Range) => DiffRenderer fields File
  PatchRenderer :: HasField fields Range => DiffRenderer fields File
  JSONDiffRenderer :: (ToJSONFields (Record fields), HasField fields Range) => DiffRenderer fields (Map Text Value)
  SummaryRenderer :: HasDefaultFields fields => DiffRenderer fields Summaries
  SExpressionDiffRenderer :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> DiffRenderer fields ByteString
  ToCRenderer :: HasDefaultFields fields => DiffRenderer fields Summaries

resolveDiffRenderer :: (Monoid output, StringConv output ByteString) => DiffRenderer fields output -> (Both SourceBlob -> Diff (Syntax Text) (Record fields) -> output)
resolveDiffRenderer renderer = case renderer of
  SplitRenderer -> (File .) . R.split
  PatchRenderer -> (File .) . R.patch
  JSONDiffRenderer -> R.json
  SummaryRenderer -> R.summary
  SExpressionDiffRenderer format -> R.sExpression format
  ToCRenderer -> R.toc

runDiffRenderer :: (Monoid output, StringConv output ByteString) => DiffRenderer fields output -> [(Both SourceBlob, Diff (Syntax Text) (Record fields))] -> output
runDiffRenderer = foldMap . uncurry . resolveDiffRenderer


data ParseTreeRenderer fields output where
  SExpressionParseTreeRenderer :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> ParseTreeRenderer fields ByteString
  JSONParseTreeRenderer :: ToJSONFields (Record fields) => Bool -> ParseTreeRenderer fields Value
  JSONIndexParseTreeRenderer :: ToJSONFields (Record fields) => Bool -> ParseTreeRenderer fields Value

resolveParseTreeRenderer :: (Monoid output, StringConv output ByteString) => ParseTreeRenderer fields output -> SourceBlob -> Term (Syntax Text) (Record fields) -> output
resolveParseTreeRenderer renderer = case renderer of
  SExpressionParseTreeRenderer format -> R.sExpressionParseTree format
  JSONParseTreeRenderer debug -> R.jsonParseTree
  JSONIndexParseTreeRenderer debug -> R.jsonIndexParseTree

runParseTreeRenderer :: (Monoid output, StringConv output ByteString) => ParseTreeRenderer fields output -> [(SourceBlob, Term (Syntax Text) (Record fields))] -> output
runParseTreeRenderer = foldMap . uncurry . resolveParseTreeRenderer


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
  showsPrec d (JSONParseTreeRenderer debug) = showsUnaryWith showsPrec "JSONParseTreeRenderer" d debug
  showsPrec d (JSONIndexParseTreeRenderer debug) = showsUnaryWith showsPrec "JSONIndexParseTreeRenderer" d debug

instance Monoid File where
  mempty = File mempty
  mappend (File a) (File b) = File (a <> "\n" <> b)
