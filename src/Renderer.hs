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

import Data.Aeson (Value, (.=))
import Data.Functor.Both hiding (fst, snd)
import Data.Functor.Classes
import Text.Show
import Data.Map as Map hiding (null)
import Data.Record
import Diff
import Info hiding (Identifier)
import Language.Ruby.Syntax (RAlgebra, decoratorWithAlgebra)
import Prologue
import Renderer.JSON as R
import Renderer.Patch as R
import Renderer.SExpression as R
import Renderer.Summary as R
import Renderer.TOC as R
import Source (SourceBlob(..))
import Syntax as S
import Term


data DiffRenderer fields output where
  PatchRenderer :: HasField fields Range => DiffRenderer fields File
  JSONDiffRenderer :: (ToJSONFields (Record fields), HasField fields Range) => DiffRenderer fields (Map Text Value)
  SummaryRenderer :: HasDefaultFields fields => DiffRenderer fields Summaries
  SExpressionDiffRenderer :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> DiffRenderer fields ByteString
  ToCRenderer :: HasDefaultFields fields => DiffRenderer fields Summaries

resolveDiffRenderer :: (Monoid output, StringConv output ByteString) => DiffRenderer fields output -> (Both SourceBlob -> Diff (Syntax Text) (Record fields) -> output)
resolveDiffRenderer renderer = case renderer of
  PatchRenderer -> (File .) . R.patch
  JSONDiffRenderer -> R.json
  SummaryRenderer -> R.summary
  SExpressionDiffRenderer format -> R.sExpression format
  ToCRenderer -> R.toc

runDiffRenderer :: (Monoid output, StringConv output ByteString) => DiffRenderer fields output -> [(Both SourceBlob, Diff (Syntax Text) (Record fields))] -> output
runDiffRenderer = foldMap . uncurry . resolveDiffRenderer


data ParseTreeRenderer fields output where
  SExpressionParseTreeRenderer :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> ParseTreeRenderer fields ByteString
  JSONParseTreeRenderer :: (ToJSONFields (Record fields), HasField fields Range) => ParseTreeRenderer fields Value

resolveParseTreeRenderer :: (Monoid output, StringConv output ByteString) => ParseTreeRenderer fields output -> SourceBlob -> Term (Syntax Text) (Record fields) -> output
resolveParseTreeRenderer renderer blob = case renderer of
  SExpressionParseTreeRenderer format -> R.sExpressionParseTree format blob
  JSONParseTreeRenderer -> R.jsonFile blob . decoratorWithAlgebra identifierAlg
  where identifierAlg :: RAlgebra (CofreeF (Syntax Text) a) (Cofree (Syntax Text) a) (Maybe Identifier)
        identifierAlg (_ :< syntax) = case syntax of
          S.Assignment f _ -> identifier f
          S.Class f _ _ -> identifier f
          S.Export f _ -> f >>= identifier
          S.Function f _ _ -> identifier f
          S.FunctionCall f _ _ -> identifier f
          S.Import f _ -> identifier f
          S.Method _ f _ _ _ -> identifier f
          S.MethodCall _ f _ _ -> identifier f
          S.Module f _ -> identifier f
          S.OperatorAssignment f _ -> identifier f
          S.SubscriptAccess f _  -> identifier f
          S.TypeDecl f _ -> identifier f
          S.VarAssignment f _ -> asum $ identifier <$> f
          _ -> Nothing
          where identifier = fmap Identifier . extractLeafValue . unwrap . fst


newtype Identifier = Identifier Text
  deriving (Eq, Show)

instance ToJSONFields Identifier where
  toJSONFields (Identifier i) = ["identifier" .= i]


runParseTreeRenderer :: (Monoid output, StringConv output ByteString) => ParseTreeRenderer fields output -> [(SourceBlob, Term (Syntax Text) (Record fields))] -> output
runParseTreeRenderer = foldMap . uncurry . resolveParseTreeRenderer


newtype File = File { unFile :: Text }
  deriving Show

instance StringConv File ByteString where
  strConv _ = encodeUtf8 . unFile

instance Show (DiffRenderer fields output) where
  showsPrec _ PatchRenderer = showString "PatchRenderer"
  showsPrec _ JSONDiffRenderer = showString "JSONDiffRenderer"
  showsPrec _ SummaryRenderer = showString "SummaryRenderer"
  showsPrec d (SExpressionDiffRenderer format) = showsUnaryWith showsPrec "SExpressionDiffRenderer" d format
  showsPrec _ ToCRenderer = showString "ToCRenderer"

instance Show (ParseTreeRenderer fields output) where
  showsPrec d (SExpressionParseTreeRenderer format) = showsUnaryWith showsPrec "SExpressionParseTreeRenderer" d format
  showsPrec _ JSONParseTreeRenderer = showString "JSONParseTreeRenderer"

instance Monoid File where
  mempty = File mempty
  mappend (File a) (File b) = File (a <> "\n" <> b)
