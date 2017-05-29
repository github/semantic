{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeOperators #-}
module Renderer
( DiffRenderer(..)
, TermRenderer(..)
, renderPatch
, renderSExpressionDiff
, renderSExpressionTerm
, renderJSON
, renderToC
, declarationDecorator
, identifierDecorator
, Summaries(..)
, File(..)
) where

import Data.Aeson (Value, (.=))
import Text.Show
import Data.Record
import Data.Syntax.Algebra (RAlgebra, decoratorWithAlgebra)
import Info hiding (Identifier)
import Prologue
import Renderer.JSON as R
import Renderer.Patch as R
import Renderer.SExpression as R
import Renderer.TOC as R
import Source (Source)
import Syntax as S
import Term

data DiffRenderer output where
  PatchDiffRenderer :: DiffRenderer File
  ToCDiffRenderer :: DiffRenderer Summaries
  JSONDiffRenderer :: DiffRenderer [Value]
  SExpressionDiffRenderer :: DiffRenderer ByteString

deriving instance Eq (DiffRenderer output)
deriving instance Show (DiffRenderer output)

data TermRenderer output where
  JSONTermRenderer :: TermRenderer [Value]
  SExpressionTermRenderer :: TermRenderer ByteString

deriving instance Eq (TermRenderer output)
deriving instance Show (TermRenderer output)


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
