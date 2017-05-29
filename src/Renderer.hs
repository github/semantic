{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators #-}
module Renderer
( Renderer(..)
, runRenderer
, declarationDecorator
, identifierDecorator
, Summaries(..)
, File(..)
) where

import Data.Aeson (ToJSON, Value, (.=))
import Data.Functor.Both hiding (fst, snd)
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
  PatchRenderer :: (HasField fields Range, Traversable f) => Renderer (Both SourceBlob, Diff f (Record fields)) File
  JSONRenderer :: (ToJSON a, Foldable t) => Renderer (t SourceBlob, a) [Value]
  SExpressionDiffRenderer :: (HasField fields Category, HasField fields SourceSpan, Foldable f) => Renderer (Both SourceBlob, Diff f (Record fields)) ByteString
  ToCRenderer :: (HasField fields Category, HasField fields (Maybe Declaration), HasField fields SourceSpan) => Renderer (Both SourceBlob, Diff (Syntax Text) (Record fields)) Summaries
  SExpressionParseTreeRenderer :: (HasField fields Category, HasField fields SourceSpan, Foldable f) => Renderer (Identity SourceBlob, Term f (Record fields)) ByteString

runRenderer :: Renderer input output -> input -> output
runRenderer renderer = case renderer of
  PatchRenderer -> File . uncurry R.patch
  JSONRenderer -> uncurry R.json
  SExpressionDiffRenderer -> R.renderSExpressionDiff . snd
  ToCRenderer -> uncurry R.toc
  SExpressionParseTreeRenderer -> R.renderSExpressionTerm . snd


declarationDecorator :: HasField fields Range => Source -> Term (Syntax Text) (Record fields) -> Term (Syntax Text) (Record (Maybe Declaration ': fields))
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

newtype File = File { unFile :: ByteString }
  deriving Show

instance StringConv File ByteString where
  strConv _ = unFile

instance Show (Renderer input output) where
  showsPrec _ PatchRenderer = showString "PatchRenderer"
  showsPrec _ JSONRenderer = showString "JSONRenderer"
  showsPrec _ SExpressionDiffRenderer = showString "SExpressionDiffRenderer"
  showsPrec _ ToCRenderer = showString "ToCRenderer"
  showsPrec _ SExpressionParseTreeRenderer = showString "SExpressionParseTreeRenderer"

instance Monoid File where
  mempty = File mempty
  mappend (File a) (File b) = File (a <> "\n" <> b)
