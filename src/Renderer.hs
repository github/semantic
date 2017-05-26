{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators #-}
module Renderer
( Renderer(..)
, SExpressionFormat(..)
, runRenderer
, declarationDecorator
, identifierDecorator
, Summaries(..)
, File(..)
) where

import Data.Aeson (ToJSON, Value, (.=))
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
  JSONRenderer :: (ToJSON a, Foldable t) => Renderer (t SourceBlob, a) [Value]
  SExpressionDiffRenderer :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> Renderer (Both SourceBlob, Diff (Syntax Text) (Record fields)) ByteString
  ToCRenderer :: (HasField fields Category, HasField fields (Maybe Declaration), HasField fields SourceSpan) => Renderer (Both SourceBlob, Diff (Syntax Text) (Record fields)) Summaries
  SExpressionParseTreeRenderer :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> Renderer (Identity SourceBlob, Term (Syntax Text) (Record fields)) ByteString

runRenderer :: (Monoid output, StringConv output ByteString) => Renderer input output -> input -> output
runRenderer renderer input = case renderer of
  PatchRenderer -> File (uncurry R.patch input)
  JSONRenderer -> uncurry R.json input
  SExpressionDiffRenderer format -> uncurry (R.sExpression format) input
  ToCRenderer -> uncurry R.toc input
  SExpressionParseTreeRenderer format -> uncurry (R.sExpressionParseTree format) (first runIdentity input)


declarationDecorator :: Source -> Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record (Maybe Declaration ': DefaultFields))
declarationDecorator = decoratorWithAlgebra . declarationAlgebra

identifierDecorator :: Term (Syntax Text) (Record fields) -> Term (Syntax Text) (Record (Maybe Identifier ': fields))
identifierDecorator = decoratorWithAlgebra identifierAlg
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
  deriving (Eq, NFData, Show)

instance ToJSONFields Identifier where
  toJSONFields (Identifier i) = ["identifier" .= i]

newtype File = File { unFile :: Text }
  deriving Show

instance StringConv File ByteString where
  strConv _ = encodeUtf8 . unFile

instance Show (Renderer input output) where
  showsPrec _ PatchRenderer = showString "PatchRenderer"
  showsPrec _ JSONRenderer = showString "JSONRenderer"
  showsPrec d (SExpressionDiffRenderer format) = showsUnaryWith showsPrec "SExpressionDiffRenderer" d format
  showsPrec _ ToCRenderer = showString "ToCRenderer"
  showsPrec d (SExpressionParseTreeRenderer format) = showsUnaryWith showsPrec "SExpressionParseTreeRenderer" d format

instance Monoid File where
  mempty = File mempty
  mappend (File a) (File b) = File (a <> "\n" <> b)
