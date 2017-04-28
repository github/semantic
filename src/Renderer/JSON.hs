{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, MultiParamTypeClasses  #-}
{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Renderer.JSON
( json
, jsonFile
, ToJSONFields(..)
) where

import Alignment
import Data.Aeson (ToJSON, toJSON, encode, object, (.=))
import Data.Aeson as A hiding (json)
import Data.Aeson.Types (emptyArray)
import Data.Bifunctor.Join
import Data.Functor.Both
import Data.Record
import Data.These
import Data.Vector as Vector hiding (toList)
import Diff
import Info
import Prologue
import qualified Data.Map as Map
import Source
import SplitDiff
import Syntax as S

--
-- Diffs
--

-- | Render a diff to a string representing its JSON.
json :: (ToJSONFields (Record fields), HasField fields Range) => Both SourceBlob -> Diff (Syntax Text) (Record fields) -> Map Text Value
json blobs diff = Map.fromList [
  ("rows", toJSON (annotateRows (alignDiff (source <$> blobs) diff))),
  ("oids", toJSON (oid <$> blobs)),
  ("paths", toJSON (path <$> blobs)) ]
  where annotateRows :: [Join These a] -> [Join These (NumberedLine a)]
        annotateRows = fmap (fmap NumberedLine) . numberedRows

-- | A numbered 'a'.
newtype NumberedLine a = NumberedLine (Int, a)

instance StringConv (Map Text Value) ByteString where
  strConv _ = toS . (<> "\n") . encode

instance ToJSONFields a => ToJSON (NumberedLine a) where
  toJSON (NumberedLine (n, a)) = object $ "number" .= n : toJSONFields a
  toEncoding (NumberedLine (n, a)) = pairs $ "number" .= n <> mconcat (toJSONFields a)

instance ToJSON a => ToJSON (Join These a) where
  toJSON (Join vs) = A.Array . Vector.fromList $ toJSON <$> these pure pure (\ a b -> [ a, b ]) vs
  toEncoding = foldable

instance ToJSON a => ToJSON (Join (,) a) where
  toJSON (Join (a, b)) = A.Array . Vector.fromList $ toJSON <$> [ a, b ]

instance (ToJSONFields a, ToJSONFields (f (Free f a))) => ToJSON (Free f a) where
  toJSON splitDiff = case runFree splitDiff of
    (Free f) -> object (toJSONFields f)
    (Pure p) -> object (toJSONFields p)
  toEncoding splitDiff = case runFree splitDiff of
    (Free f) -> pairs $ mconcat (toJSONFields f)
    (Pure p) -> pairs $ mconcat (toJSONFields p)

instance ToJSONFields (CofreeF f a (Cofree f a)) => ToJSON (Cofree f a) where
  toJSON = object . toJSONFields . runCofree
  toEncoding = pairs . mconcat . toJSONFields . runCofree

class ToJSONFields a where
  toJSONFields :: KeyValue kv => a -> [kv]

instance (ToJSONFields h, ToJSONFields (Record t)) => ToJSONFields (Record (h ': t)) where
  toJSONFields (h :. t) = toJSONFields h <> toJSONFields t

instance ToJSONFields (Record '[]) where
  toJSONFields _ = []

instance ToJSONFields Range where
  toJSONFields Range{..} = ["range" .= [ start, end ]]

instance ToJSONFields Category where
  toJSONFields c = ["category" .= case c of { Other s -> s ; _ -> toS c }]

instance ToJSONFields SourceSpan where
  toJSONFields sourceSpan = [ "sourceSpan" .= sourceSpan ]

instance ToJSONFields SourceText where
  toJSONFields (SourceText t) = [ "sourceText" .= t ]

instance ToJSONFields a => ToJSONFields (Maybe a) where
  toJSONFields = maybe [] toJSONFields

instance (ToJSONFields a, ToJSONFields (f (Cofree f a))) => ToJSONFields (Cofree f a) where
  toJSONFields = toJSONFields . runCofree

instance (ToJSONFields a, ToJSONFields (f b)) => ToJSONFields (CofreeF f a b) where
  toJSONFields (a :< f) = toJSONFields a <> toJSONFields f

instance (ToJSONFields a, ToJSONFields (f (Free f a))) => ToJSONFields (Free f a) where
  toJSONFields = toJSONFields . runFree

instance (ToJSONFields a, ToJSONFields (f b)) => ToJSONFields (FreeF f a b) where
  toJSONFields (Free f) = toJSONFields f
  toJSONFields (Pure a) = toJSONFields a

instance ToJSON a => ToJSONFields (SplitPatch a) where
  toJSONFields (SplitInsert a) = [ "insert" .= a ]
  toJSONFields (SplitDelete a) = [ "delete" .= a ]
  toJSONFields (SplitReplace a) = [ "replace" .= a ]

instance ToJSON recur => ToJSONFields (Syntax leaf recur) where
  toJSONFields syntax = case syntax of
    Leaf _ -> []
    Indexed c -> childrenFields c
    Fixed c -> childrenFields c
    S.FunctionCall identifier typeParameters parameters -> [ "identifier" .= identifier, "typeArguments" .= typeParameters, "parameters" .= parameters ]
    S.Ternary expression cases -> [ "expression" .= expression, "cases" .= cases ]
    S.AnonymousFunction callSignature c -> "callSignature" .= callSignature : childrenFields c
    S.Function identifier callSignature c -> "identifier" .= identifier : "callSignature" .= callSignature : childrenFields c
    S.Assignment assignmentId value -> [ "identifier" .= assignmentId, "value" .= value ]
    S.OperatorAssignment identifier value -> [ "identifier" .= identifier, "value" .= value ]
    S.MemberAccess identifier value -> [ "identifier" .= identifier, "value" .= value ]
    S.MethodCall identifier methodIdentifier typeParameters parameters -> [ "identifier" .= identifier, "methodIdentifier" .= methodIdentifier, "typeParameters" .= typeParameters, "parameters" .= parameters ]
    S.Operator syntaxes -> [ "operatorSyntaxes" .= syntaxes ]
    S.VarDecl children -> childrenFields children
    S.VarAssignment identifier value -> [ "identifier" .= identifier, "value" .= value ]
    S.SubscriptAccess identifier property -> [ "identifier" .= identifier, "property" .= property ]
    S.Switch expression cases -> [ "expression" .= expression, "cases" .= cases ]
    S.Case expression statements -> [ "expression" .= expression, "statements" .= statements ]
    S.Object ty keyValuePairs -> "type" .= ty : childrenFields keyValuePairs
    S.Pair a b -> childrenFields [a, b]
    S.Comment _ -> []
    S.Commented comments child -> childrenFields (comments <> maybeToList child)
    S.ParseError c -> childrenFields c
    S.For expressions body -> [ "expressions" .= expressions, "body" .= body ]
    S.DoWhile expression body -> [ "expression" .= expression, "body" .= body ]
    S.While expression body -> [ "expression" .= expression, "body" .= body ]
    S.Return expression -> [ "expression" .= expression ]
    S.Throw c -> [ "expression" .= c ]
    S.Constructor expression -> [ "expression" .= expression ]
    S.Try body catchExpression elseExpression finallyExpression -> [ "body" .= body, "catchExpression" .= catchExpression, "elseExpression" .= elseExpression, "finallyExpression" .= finallyExpression ]
    S.Array ty c -> "type" .= ty : childrenFields c
    S.Class identifier superclass definitions -> [ "identifier" .= identifier, "superclass" .= superclass, "definitions" .= definitions ]
    S.Method clauses identifier receiver callSignature definitions -> [ "clauses" .= clauses, "identifier" .= identifier, "receiver" .= receiver, "callSignature" .= callSignature, "definitions" .= definitions ]
    S.If expression clauses -> "expression" .= expression : childrenFields clauses
    S.Module identifier definitions -> [ "identifier" .= identifier, "definitions" .= definitions ]
    S.Namespace identifier definitions -> [ "identifier" .= identifier, "definitions" .= definitions ]
    S.Interface identifier clauses definitions -> [ "identifier" .= identifier, "clauses" .= clauses, "definitions" .= definitions ]
    S.Import identifier statements -> [ "identifier" .= identifier, "statements" .= statements ]
    S.Export identifier statements -> [ "identifier" .= identifier, "statements" .= statements ]
    S.Yield expr -> [ "yieldExpression" .= expr ]
    S.Negate expr -> [ "negate" .= expr ]
    S.Rescue args expressions -> "args" .= args : childrenFields expressions
    S.Select cases -> childrenFields cases
    S.Go cases -> childrenFields cases
    S.Defer cases -> childrenFields cases
    S.TypeAssertion a b -> childrenFields [a, b]
    S.TypeConversion a b -> childrenFields [a, b]
    S.Struct ty fields -> "type" .= ty : childrenFields fields
    S.Break expr -> [ "expression" .= expr ]
    S.Continue expr -> [ "expression" .= expr ]
    S.BlockStatement c -> childrenFields c
    S.ParameterDecl ty field -> [ "type" .= ty, "identifier" .= field ]
    S.DefaultCase c -> childrenFields c
    S.TypeDecl id ty -> [ "type" .= ty, "identifier" .= id ]
    S.FieldDecl children -> childrenFields children
    S.Ty ty -> [ "type" .= ty ]
    S.Send channel expr -> [ "channel" .= channel, "expression" .= expr ]
    where childrenFields c = [ "children" .= c ]


--
-- Parse Trees
--

data File a = File { filePath :: FilePath, fileContent :: a }
  deriving (Generic, Show)

instance ToJSON a => ToJSON (File a) where
  toJSON File{..} = object [ "filePath" .= filePath, "programNode" .= fileContent ]

instance Monoid Value where
  mempty = emptyArray
  mappend a b = A.Array $ Vector.fromList [a, b]

instance StringConv Value ByteString where
  strConv _ = toS . (<> "\n") . encode

jsonFile :: ToJSON a => SourceBlob -> a -> Value
jsonFile SourceBlob{..} = toJSON . File path
