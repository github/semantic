{-# LANGUAGE DataKinds, GADTs, MultiParamTypeClasses, TypeOperators #-}
module Renderer
( Renderer(..)
, SExpressionFormat(..)
, resolveRenderer
, runRenderer
, declarationDecorator
, Summaries(..)
, File(..)
) where

import Data.Aeson (Value, (.=))
import Data.Functor.Both hiding (fst, snd)
import Data.Functor.Classes
import Text.Show
import Data.Record
import Data.Syntax.Algebra (RAlgebra, decoratorWithAlgebra)
import Diff
import Info hiding (Identifier)
import Prologue
import Renderer.JSON as R
import Renderer.Patch as R
import Renderer.SExpression as R
import Renderer.TOC as R
import Source (SourceBlob(..), Source)
import Syntax as S
import Term


data Renderer input output where
  PatchRenderer :: HasField fields Range => Renderer (Both SourceBlob, Diff (Syntax Text) (Record fields)) File
  JSONDiffRenderer :: ToJSONFields (Record fields) => Renderer (Both SourceBlob, Diff (Syntax Text) (Record fields)) [Value]
  SExpressionDiffRenderer :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> Renderer (Both SourceBlob, Diff (Syntax Text) (Record fields)) ByteString
  ToCRenderer :: (HasField fields Category, HasField fields (Maybe Declaration), HasField fields SourceSpan) => Renderer (Both SourceBlob, Diff (Syntax Text) (Record fields)) Summaries
  SExpressionParseTreeRenderer :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> Renderer (SourceBlob, Term (Syntax Text) (Record fields)) ByteString
  JSONParseTreeRenderer :: ToJSONFields (Record fields) => Renderer (SourceBlob, Term (Syntax Text) (Record fields)) [Value]

resolveRenderer :: (Monoid output, StringConv output ByteString) => Renderer input output -> input -> output
resolveRenderer renderer input = case renderer of
  PatchRenderer -> File (uncurry R.patch input)
  JSONDiffRenderer -> uncurry R.json input
  SExpressionDiffRenderer format -> uncurry (R.sExpression format) input
  ToCRenderer -> uncurry R.toc input
  SExpressionParseTreeRenderer format -> uncurry (R.sExpressionParseTree format) input
  JSONParseTreeRenderer -> let (blob, term) = input in R.jsonFile blob (decoratorWithAlgebra identifierAlg term)
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

runRenderer :: (Monoid output, StringConv output ByteString) => Renderer input output -> [input] -> output
runRenderer = foldMap . resolveRenderer


declarationDecorator :: Source -> Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record (Maybe Declaration ': DefaultFields))
declarationDecorator = decoratorWithAlgebra . declarationAlgebra


newtype Identifier = Identifier Text
  deriving (Eq, Show)

instance ToJSONFields Identifier where
  toJSONFields (Identifier i) = ["identifier" .= i]

newtype File = File { unFile :: Text }
  deriving Show

instance StringConv File ByteString where
  strConv _ = encodeUtf8 . unFile

instance Show (Renderer input output) where
  showsPrec _ PatchRenderer = showString "PatchRenderer"
  showsPrec _ JSONDiffRenderer = showString "JSONDiffRenderer"
  showsPrec d (SExpressionDiffRenderer format) = showsUnaryWith showsPrec "SExpressionDiffRenderer" d format
  showsPrec _ ToCRenderer = showString "ToCRenderer"
  showsPrec d (SExpressionParseTreeRenderer format) = showsUnaryWith showsPrec "SExpressionParseTreeRenderer" d format
  showsPrec _ JSONParseTreeRenderer = showString "JSONParseTreeRenderer"

instance Monoid File where
  mempty = File mempty
  mappend (File a) (File b) = File (a <> "\n" <> b)
