{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
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
json :: (ToJSON (Record fields), HasField fields Category, HasField fields Range) => Renderer (Record fields)
json blobs diff = JSONOutput $ Map.fromList [
  ("rows", toJSON (annotateRows (alignDiff (source <$> blobs) diff))),
  ("oids", toJSON (oid <$> blobs)),
  ("paths", toJSON (path <$> blobs)) ]
  where annotateRows :: [Join These a] -> [Join These (NumberedLine a)]
        annotateRows = fmap (fmap NumberedLine) . numberedRows

-- | A numbered 'a'.
newtype NumberedLine a = NumberedLine (Int, a)

instance (ToJSON leaf, ToJSON (Record fields), HasField fields Category, HasField fields Range) => ToJSON (NumberedLine (SplitSyntaxDiff leaf fields)) where
  toJSON (NumberedLine (n, a)) = object (lineFields n a (getRange a))
  toEncoding (NumberedLine (n, a)) = pairs $ mconcat (lineFields n a (getRange a))

instance ToJSON Category where
  toJSON (Other s) = String s
  toJSON s = String . T.pack $ show s

instance ToJSON Range where
  toJSON (Range start end) = A.Array . Vector.fromList $ toJSON <$> [ start, end ]
  toEncoding (Range start end) = foldable [ start, end ]

instance ToJSON a => ToJSON (Join These a) where
  toJSON (Join vs) = A.Array . Vector.fromList $ toJSON <$> these pure pure (\ a b -> [ a, b ]) vs
  toEncoding = foldable

instance ToJSON a => ToJSON (Join (,) a) where
  toJSON (Join (a, b)) = A.Array . Vector.fromList $ toJSON <$> [ a, b ]

instance (ToJSON leaf, ToJSON (Record fields), HasField fields Category, HasField fields Range) => ToJSON (SplitSyntaxDiff leaf fields) where
  toJSON splitDiff = case runFree splitDiff of
    (Free (info :< syntax)) -> object (termFields info syntax)
    (Pure patch)            -> object (patchFields patch)
  toEncoding splitDiff = case runFree splitDiff of
    (Free (info :< syntax)) -> pairs $ mconcat (termFields info syntax)
    (Pure patch)            -> pairs $ mconcat (patchFields patch)

instance (ToJSON (Record fields), ToJSON leaf, HasField fields Category, HasField fields Range) => ToJSON (SyntaxTerm leaf fields) where
  toJSON term     |
    (info :< syntax) <- runCofree term = object (termFields info syntax)
  toEncoding term |
    (info :< syntax) <- runCofree term = pairs $ mconcat (termFields info syntax)

lineFields :: (ToJSON leaf, ToJSON (Record fields), HasField fields Category, HasField fields Range, KeyValue kv) =>
  Int ->
  SplitSyntaxDiff leaf fields ->
  Range ->
  [kv]
lineFields n term range = [ "number" .= n
                          , "terms" .= [ term ]
                          , "range" .= range
                          , "hasChanges" .= hasChanges term
                          ]

termFields :: (ToJSON recur, KeyValue kv, HasField fields Category, HasField fields Range) =>
  Record fields ->
  Syntax leaf recur ->
  [kv]
termFields info syntax = "range" .= characterRange info : "category" .= category info : syntaxToTermField syntax

patchFields :: (ToJSON (Record fields), ToJSON leaf, KeyValue kv, HasField fields Category, HasField fields Range) =>
  SplitPatch (SyntaxTerm leaf fields) ->
  [kv]
patchFields patch = case patch of
  SplitInsert term -> fields "insert" term
  SplitDelete term -> fields "delete" term
  SplitReplace term -> fields "replace" term
  where
    fields kind term |
      (info :< syntax) <- runCofree term = "patch" .= T.pack kind : termFields info syntax

syntaxToTermField :: (ToJSON recur, KeyValue kv) =>
  Syntax leaf recur ->
  [kv]
syntaxToTermField syntax = case syntax of
  Leaf _ -> []
  Indexed c -> childrenFields c
  Fixed c -> childrenFields c
  S.FunctionCall identifier parameters -> [ "identifier" .= identifier ] <> [ "parameters" .= parameters ]
  S.Ternary expression cases -> [ "expression" .= expression ] <> [ "cases" .= cases ]
  S.AnonymousFunction parameters c -> [ "parameters" .= parameters ] <> childrenFields c
  S.Function identifier parameters ty c -> [ "identifier" .= identifier ] <> [ "parameters" .= parameters ] <> [ "type" .= ty ] <> childrenFields c
  S.Assignment assignmentId value -> [ "identifier" .= assignmentId ] <> [ "value" .= value ]
  S.OperatorAssignment identifier value -> [ "identifier" .= identifier ] <> [ "value" .= value ]
  S.MemberAccess identifier value -> [ "identifier" .= identifier ] <> [ "value" .= value ]
  S.MethodCall identifier methodIdentifier parameters -> [ "identifier" .= identifier ] <> [ "methodIdentifier" .= methodIdentifier ] <> [ "parameters" .= parameters ]
  S.Operator syntaxes -> [ "operatorSyntaxes" .= syntaxes ]
  S.VarDecl declaration ty -> [ "declaration" .= declaration ] <> [ "type" .= ty]
  S.VarAssignment identifier value -> [ "identifier" .= identifier ] <> [ "value" .= value ]
  S.SubscriptAccess identifier property -> [ "identifier" .= identifier ] <> [ "property" .= property ]
  S.Switch expression cases -> [ "expression" .= expression ] <> [ "cases" .= cases ]
  S.Case expression statements -> [ "expression" .= expression ] <> [ "statements" .= statements ]
  S.Object ty keyValuePairs -> [ "type" .= ty ] <> childrenFields keyValuePairs
  S.Pair a b -> childrenFields [a, b]
  S.Comment _ -> []
  S.Commented comments child -> childrenFields (comments <> maybeToList child)
  S.Error c -> childrenFields c
  S.For expressions body -> [ "expressions" .= expressions ] <> [ "body" .= body ]
  S.DoWhile expression body -> [ "expression" .= expression ]  <> [ "body" .= body ]
  S.While expression body -> [ "expression" .= expression ]  <> [ "body" .= body ]
  S.Return expression -> [ "expression" .= expression ]
  S.Throw c -> [ "expression" .= c ]
  S.Constructor expression -> [ "expression" .= expression ]
  S.Try body catchExpression elseExpression finallyExpression -> [ "body" .= body ] <> [ "catchExpression" .= catchExpression ] <> [ "elseExpression" .= elseExpression ] <> [ "finallyExpression" .= finallyExpression ]
  S.Array ty c -> [ "type" .= ty ] <> childrenFields c
  S.Class identifier superclass definitions -> [ "identifier" .= identifier ] <> [ "superclass" .= superclass ] <> [ "definitions" .= definitions ]
  S.Method identifier ty parameters definitions -> [ "identifier" .= identifier ] <> [ "type" .= ty ] <> [ "parameters" .= parameters ] <> [ "definitions" .= definitions ]
  S.If expression clauses -> [ "expression" .= expression ] <> childrenFields clauses
  S.Module identifier definitions-> [ "identifier" .= identifier ] <> [ "definitions" .= definitions ]
  S.Import identifier statements -> [ "identifier" .= identifier ] <> [ "statements" .= statements ]
  S.Export identifier statements -> [ "identifier" .= identifier ] <> [ "statements" .= statements ]
  S.Yield expr -> [ "yieldExpression" .= expr ]
  S.Negate expr -> [ "negate" .= expr ]
  S.Rescue args expressions -> [ "args" .= args ] <> childrenFields expressions
  S.Select cases -> childrenFields cases
  S.Go cases -> childrenFields cases
  S.Defer cases -> childrenFields cases
  S.TypeAssertion a b -> childrenFields [a, b]
  S.TypeConversion a b -> childrenFields [a, b]
  S.Struct ty fields -> [ "type" .= ty ] <> childrenFields fields
  S.Break expr -> [ "expression" .= expr ]
  S.Continue expr -> [ "expression" .= expr ]
  S.BlockStatement c -> childrenFields c
  S.ParameterDecl ty field -> [ "type" .= ty ] <> [ "identifier" .= field ]
  S.DefaultCase c -> childrenFields c
  S.TypeDecl id ty -> [ "type" .= ty ] <> [ "identifier" .= id ]
  S.FieldDecl id ty tag -> [ "type" .= ty ] <> [ "identifier" .= id ] <> [ "tag" .= tag]
  S.Ty ty -> [ "type" .= ty ]
  S.Send channel expr -> [ "channel" .= channel ] <> [ "expression" .= expr ]
  where childrenFields c = [ "children" .= c ]
