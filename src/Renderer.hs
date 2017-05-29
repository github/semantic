{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeOperators #-}
module Renderer
( DiffRenderer(..)
, TermRenderer(..)
, renderPatch
, renderSExpressionDiff
, renderSExpressionTerm
, renderJSON
, renderToC
, declarationAlgebra
, identifierAlgebra
, Summaries(..)
, File(..)
) where

import Data.Aeson (Value, (.=))
import Text.Show
import Data.Syntax.Algebra (RAlgebra)
import Prologue
import Renderer.JSON as R
import Renderer.Patch as R
import Renderer.SExpression as R
import Renderer.TOC as R
import Source (Source)
import Syntax as S

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
  SourceTermRenderer :: TermRenderer Source

deriving instance Eq (TermRenderer output)
deriving instance Show (TermRenderer output)


identifierAlgebra :: RAlgebra (CofreeF (Syntax Text) a) (Cofree (Syntax Text) a) (Maybe Identifier)
identifierAlgebra (_ :< syntax) = case syntax of
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
