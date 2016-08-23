{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, DeriveAnyClass, StandaloneDeriving, ScopedTypeVariables, TypeOperators, DataKinds, KindSignatures, GADTs, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Renderer.JSON (
  json
) where

import Prologue hiding (toList)
import Alignment
import Category
import Data.Aeson as A hiding (json)
import Data.Bifunctor.Join
import Data.Record
import qualified Data.Text as T
import Data.These
import Data.Vector as Vector hiding (toList)
import Info
import Renderer
import Source hiding (fromList)
import SplitDiff
import Syntax as S
import Term
import qualified Data.Map as Map

-- | Render a diff to a string representing its JSON.
json :: forall fields. (ToJSON Category, ToJSON Range, HasField fields Category, HasField fields Range) => Renderer (Record fields)
json blobs diff = JSONOutput $ Map.fromList [ ("rows", toJSON $ annotateRows (alignDiff (source <$> blobs) diff) )
  , ("oids", toJSON (oid <$> blobs)), ("paths", toJSON (path <$> blobs))]
  where annotateRows = fmap (fmap NumberedLine) . numberedRows

-- | A numbered 'a'.
newtype NumberedLine a = NumberedLine (Int, a)

instance (HasField fields Category, HasField fields Range) => ToJSON (NumberedLine (SplitDiff leaf (Record fields))) where
  toJSON (NumberedLine (n, a)) = object (lineFields n a (getRange a))
  toEncoding (NumberedLine (n, a)) = pairs $ mconcat (lineFields n a (getRange a))
instance ToJSON Category where
  toJSON (Other s) = String s
  toJSON s = String . T.pack $ show s
instance ToJSON Range where
  toJSON (Range start end) = A.Array . Vector.fromList $ toJSON <$> [ start, end ]
  toEncoding (Range start end) = foldable [ start,  end ]
instance ToJSON a => ToJSON (Join These a) where
  toJSON (Join vs) = A.Array . Vector.fromList $ toJSON <$> these pure pure (\ a b -> [ a, b ]) vs
  toEncoding = foldable
instance ToJSON a => ToJSON (Join (,) a) where
  toJSON (Join (a, b)) = A.Array . Vector.fromList $ toJSON <$> [ a, b ]
instance (HasField fields Category, HasField fields Range) => ToJSON (SplitDiff leaf (Record fields)) where
  toJSON splitDiff = case runFree splitDiff of
    (Free (info :< syntax)) -> object (termFields info syntax)
    (Pure patch)            -> object (patchFields patch)
  toEncoding splitDiff = case runFree splitDiff of
    (Free (info :< syntax)) -> pairs $ mconcat (termFields info syntax)
    (Pure patch)            -> pairs $ mconcat (patchFields patch)
instance (HasField fields Category, HasField fields Range) => ToJSON (Term leaf (Record fields)) where
  toJSON term     | (info :< syntax) <- runCofree term = object (termFields info syntax)
  toEncoding term | (info :< syntax) <- runCofree term = pairs $ mconcat (termFields info syntax)

lineFields :: (HasField fields Category, HasField fields Range) => KeyValue kv => Int -> SplitDiff leaf (Record fields) -> Range -> [kv]
lineFields n term range = [ "number" .= n
                          , "terms" .= [ term ]
                          , "range" .= range
                          , "hasChanges" .= hasChanges term
                          ]

termFields :: (ToJSON recur, KeyValue kv, HasField fields Category, HasField fields Range) => Record fields -> Syntax leaf recur -> [kv]
termFields info syntax = "range" .= characterRange info : "category" .= category info : case syntax of
  Leaf _ -> []
  Indexed c -> childrenFields c
  Fixed c -> childrenFields c
  S.AnonymousFunction params c -> [ "params" .= params ] <> childrenFields c
  S.FunctionCall identifier params -> [ "identifier" .= identifier ] <> [ "params" .= params ]
  S.Function identifier params c -> [ "identifier" .= identifier ] <> [ "params" .= params ] <> childrenFields c
  S.MethodCall targetId methodId args -> [ "targetIdentifier" .= targetId ] <> [ "methodId" .= methodId ] <> [ "args" .= args ]
  S.Args c -> childrenFields c
  S.Assignment assignmentId property -> [ "assignmentIdentifier" .= assignmentId ] <> [ "property" .= property ]
  S.MemberAccess memberId value -> [ "memberIdentifier" .= memberId ] <> [ "value" .= value ]
  S.If expr clause maybeClause -> [ "if" .= expr ] <> [ "ifBody" .=  clause ] <> [ "elseBody" .= maybeClause ]
  S.For exprs body -> [ "forExpressions" .= exprs ] <> [ "forBody" .= body ]
  S.While expr body -> [ "whileExpr" .= expr ]  <> [ "whileBody" .= body ]
  S.DoWhile expr body -> [ "doWhileExpr" .= expr ]  <> [ "doWhileBody" .= body ]
  S.Switch expr cases -> [ "switchExpression" .= expr ] <> [ "cases" .= cases ]
  S.Case expr body -> [ "caseExpression" .= expr ] <> [ "caseStatement" .= body ]
  S.VarDecl decl -> [ "variableDeclaration" .= decl ]
  S.VarAssignment id value -> [ "varIdentifier" .= id ] <> [ "value" .= value ]
  S.MathAssignment id value -> [ "mathIdentifier" .= id ] <> [ "value" .= value ]
  S.Ternary expr cases -> [ "ternaryExpression" .= expr ] <> [ "cases" .= cases ]
  S.Operator syntaxes -> [ "operatorSyntaxes" .= syntaxes ]
  S.SubscriptAccess id property -> [ "subscriptId" .= id ] <> [ "property" .= property ]
  S.Object pairs -> childrenFields pairs
  S.Pair a b -> childrenFields [a, b]
  S.Return expr -> [ "returnExpression" .= expr ]
  S.Constructor expr -> [ "constructorExpression" .= expr ]
  S.Comment _ -> []
  S.Commented comments child -> childrenFields (comments <> maybeToList child)
  S.Error sourceSpan c -> [ "sourceSpan" .= sourceSpan ] <> childrenFields c
  S.Throw c -> [ "throwExpression" .= c ]
  S.Try body catch finally -> [ "tryBody" .= body ] <> [ "tryCatch" .= catch ] <> [ "tryFinally" .= finally ]
  S.Array c -> childrenFields c
  S.Class identifier superclass definitions -> [ "classIdentifier" .= identifier ] <> [ "superclass" .= superclass ] <> [ "definitions" .= definitions ]
  S.Method identifier params definitions -> [ "methodIdentifier" .= identifier ] <> [ "params" .= params ] <> [ "definitions" .= definitions ]
  where childrenFields c = [ "children" .= c ]

patchFields :: (KeyValue kv, HasField fields Category, HasField fields Range) => SplitPatch (Term leaf (Record fields)) -> [kv]
patchFields patch = case patch of
  SplitInsert term -> fields "insert" term
  SplitDelete term -> fields "delete" term
  SplitReplace term -> fields "replace" term
  where fields kind term | (info :< syntax) <- runCofree term = "patch" .= T.pack kind : termFields info syntax
