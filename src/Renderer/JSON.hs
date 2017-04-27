{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, MultiParamTypeClasses  #-}
{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Renderer.JSON
( json
, jsonParseTree
, jsonIndexParseTree
, ParseTreeFile(..)
) where

import Alignment
import Data.Aeson (ToJSON, toJSON, encode, object, (.=))
import Data.Aeson as A hiding (json)
import Data.Aeson.Types (Pair, emptyArray)
import Data.Bifunctor.Join
import Data.Functor.Both
import Data.Functor.Foldable hiding (Nil)
import Data.Record
import Data.These
import Data.Vector as Vector hiding (toList)
import Diff
import Info
import Language.Ruby.Syntax (decoratorWithAlgebra, fToR)
import Prologue
import qualified Data.Map as Map
import qualified Data.Text as T
import Source
import SplitDiff
import Syntax as S
import Term

--
-- Diffs
--

-- | Render a diff to a string representing its JSON.
json :: (ToJSON (Record fields), HasField fields Category, HasField fields Range) => Both SourceBlob -> Diff (Syntax Text) (Record fields) -> Map Text Value
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

instance (ToJSON leaf, ToJSON (Record fields), HasField fields Category, HasField fields Range) => ToJSON (NumberedLine (SplitSyntaxDiff leaf fields)) where
  toJSON (NumberedLine (n, a)) = object (lineFields n a (getRange a))
  toEncoding (NumberedLine (n, a)) = pairs $ mconcat (lineFields n a (getRange a))

instance ToJSON Category where
  toJSON (Other s) = String s
  toJSON s = String (toS s)

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

instance ToJSONFields a => ToJSONFields (Maybe a) where
  toJSONFields = maybe [] toJSONFields

instance (ToJSONFields a, ToJSONFields (f (Term f a))) => ToJSONFields (Term f a) where
  toJSONFields term = let a :< f = runCofree term in toJSONFields a <> toJSONFields f

instance ToJSON a => ToJSONFields (SplitPatch a) where
  toJSONFields (SplitInsert a) = [ "insert" .= a ]
  toJSONFields (SplitDelete a) = [ "delete" .= a ]
  toJSONFields (SplitReplace a) = [ "replace" .= a ]


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
termFields info syntax = "range" .= byteRange info : "category" .= Info.category info : toJSONFields syntax

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

instance ToJSON recur => ToJSONFields (Syntax leaf recur) where
  toJSONFields syntax = case syntax of
    Leaf _ -> []
    Indexed c -> childrenFields c
    Fixed c -> childrenFields c
    S.FunctionCall identifier typeParameters parameters -> [ "identifier" .= identifier ] <> [ "typeArguments" .= typeParameters] <> [ "parameters" .= parameters ]
    S.Ternary expression cases -> [ "expression" .= expression ] <> [ "cases" .= cases ]
    S.AnonymousFunction callSignature c -> [ "callSignature" .= callSignature ] <> childrenFields c
    S.Function identifier callSignature c -> [ "identifier" .= identifier ] <> [ "callSignature" .= callSignature ] <> childrenFields c
    S.Assignment assignmentId value -> [ "identifier" .= assignmentId ] <> [ "value" .= value ]
    S.OperatorAssignment identifier value -> [ "identifier" .= identifier ] <> [ "value" .= value ]
    S.MemberAccess identifier value -> [ "identifier" .= identifier ] <> [ "value" .= value ]
    S.MethodCall identifier methodIdentifier typeParameters parameters -> [ "identifier" .= identifier ] <> [ "methodIdentifier" .= methodIdentifier ] <> [ "typeParameters" .= typeParameters ] <> [ "parameters" .= parameters ]
    S.Operator syntaxes -> [ "operatorSyntaxes" .= syntaxes ]
    S.VarDecl children -> childrenFields children
    S.VarAssignment identifier value -> [ "identifier" .= identifier ] <> [ "value" .= value ]
    S.SubscriptAccess identifier property -> [ "identifier" .= identifier ] <> [ "property" .= property ]
    S.Switch expression cases -> [ "expression" .= expression ] <> [ "cases" .= cases ]
    S.Case expression statements -> [ "expression" .= expression ] <> [ "statements" .= statements ]
    S.Object ty keyValuePairs -> [ "type" .= ty ] <> childrenFields keyValuePairs
    S.Pair a b -> childrenFields [a, b]
    S.Comment _ -> []
    S.Commented comments child -> childrenFields (comments <> maybeToList child)
    S.ParseError c -> childrenFields c
    S.For expressions body -> [ "expressions" .= expressions ] <> [ "body" .= body ]
    S.DoWhile expression body -> [ "expression" .= expression ]  <> [ "body" .= body ]
    S.While expression body -> [ "expression" .= expression ]  <> [ "body" .= body ]
    S.Return expression -> [ "expression" .= expression ]
    S.Throw c -> [ "expression" .= c ]
    S.Constructor expression -> [ "expression" .= expression ]
    S.Try body catchExpression elseExpression finallyExpression -> [ "body" .= body ] <> [ "catchExpression" .= catchExpression ] <> [ "elseExpression" .= elseExpression ] <> [ "finallyExpression" .= finallyExpression ]
    S.Array ty c -> [ "type" .= ty ] <> childrenFields c
    S.Class identifier superclass definitions -> [ "identifier" .= identifier ] <> [ "superclass" .= superclass ] <> [ "definitions" .= definitions ]
    S.Method clauses identifier receiver callSignature definitions -> [ "clauses" .= clauses ] <> [ "identifier" .= identifier ] <> [ "receiver" .= receiver ] <> [ "callSignature" .= callSignature ] <> [ "definitions" .= definitions ]
    S.If expression clauses -> [ "expression" .= expression ] <> childrenFields clauses
    S.Module identifier definitions -> [ "identifier" .= identifier ] <> [ "definitions" .= definitions ]
    S.Namespace identifier definitions -> [ "identifier" .= identifier ] <> [ "definitions" .= definitions ]
    S.Interface identifier clauses definitions -> [ "identifier" .= identifier ] <> [ "clauses" .= clauses ] <> [ "definitions" .= definitions ]
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
    S.FieldDecl children -> childrenFields children
    S.Ty ty -> [ "type" .= ty ]
    S.Send channel expr -> [ "channel" .= channel ] <> [ "expression" .= expr ]
    where childrenFields c = [ "children" .= c ]


--
-- Parse Trees
--

data ParseTreeFile = ParseTreeFile { parseTreeFilePath :: FilePath, node :: Rose ParseNode } deriving (Show)

data Rose a = Rose a [Rose a]
  deriving (Eq, Show)


instance ToJSON ParseTreeFile where
  toJSON ParseTreeFile{..} = object [ "filePath" .= parseTreeFilePath, "programNode" .= cata algebra node ]
    where algebra (RoseF a as) = object $ parseNodeToJSONFields a <> [ "children" .= as ]

instance Monoid Value where
  mempty = emptyArray
  mappend a b = A.Array $ Vector.fromList [a, b]

instance StringConv Value ByteString where
  strConv _ = toS . (<> "\n") . encode

data IndexFile = IndexFile { indexFilePath :: FilePath, nodes :: [ParseNode] } deriving (Show)

instance ToJSON IndexFile where
  toJSON IndexFile{..} = object [ "filePath" .= indexFilePath, "programNodes" .= foldMap (singleton . object . parseNodeToJSONFields) nodes ]
    where singleton a = [a]

data ParseNode = ParseNode
  { category :: Category
  , sourceRange :: Range
  , sourceText :: Maybe SourceText
  , sourceSpan :: SourceSpan
  , identifier :: Maybe Text
  }
  deriving (Show)

-- | Produce a list of JSON 'Pair's for the fields in a given ParseNode.
parseNodeToJSONFields :: ParseNode -> [Pair]
parseNodeToJSONFields ParseNode{..} =
     [ "category" .= (toS category :: Text), "sourceRange" .= sourceRange, "sourceSpan" .= sourceSpan ]
  <> [ "sourceText" .= sourceText | isJust sourceText ]
  <> [ "identifier" .= identifier | isJust identifier ]

jsonParseTree :: HasDefaultFields fields => Bool -> SourceBlob -> Term (Syntax Text) (Record fields) -> Value
jsonParseTree = jsonParseTree' ParseTreeFile Rose

jsonIndexParseTree :: HasDefaultFields fields => Bool -> SourceBlob -> Term (Syntax Text) (Record fields) -> Value
jsonIndexParseTree = jsonParseTree' IndexFile combine
  where combine node siblings = node : Prologue.concat siblings

jsonParseTree' :: (ToJSON root, HasDefaultFields fields) => (FilePath -> a -> root) -> (ParseNode -> [a] -> a) -> Bool -> SourceBlob -> Term (Syntax Text) (Record fields) -> Value
jsonParseTree' constructor combine debug SourceBlob{..} term = toJSON $ constructor path (para algebra term')
  where
    term' = decorateTerm (if debug then termSourceTextDecorator source else const Nothing) (decoratorWithAlgebra (fToR maybeIdentifier) term)
    algebra (annotation :< syntax) = combine (makeNode annotation) (toList (Prologue.snd <$> syntax))

    makeNode :: HasDefaultFields fields => Record (Maybe SourceText ': Maybe Text ': fields) -> ParseNode
    makeNode record = ParseNode (getField record) (getField record) (getField record) (getField record) (getField record)

    -- | Decorate a 'Term' using a function to compute the annotation values at every node.
    decorateTerm :: (Functor f, HasDefaultFields fields) => TermDecorator f fields field -> Term f (Record fields) -> Term f (Record (field ': fields))
    decorateTerm decorator = cata $ \ term -> cofree ((decorator term :. headF term) :< tailF term)

    -- | Term decorator extracting the source text for a term.
    termSourceTextDecorator :: HasField fields Range => Source -> TermDecorator f fields (Maybe SourceText)
    termSourceTextDecorator source (ann :< _) = Just (SourceText (toText (Source.slice (byteRange ann) source)))

-- | A function computing a value to decorate terms with. This can be used to cache synthesized attributes on terms.
type TermDecorator f fields field = TermF f (Record fields) (Term f (Record (field ': fields))) -> field

data RoseF a b = RoseF a [b]
  deriving (Eq, Functor, Show)

type instance Base (Rose a) = RoseF a

instance Recursive (Rose a) where
  project (Rose a tree) = RoseF a tree

instance Corecursive (Rose a) where
  embed (RoseF a tree) = Rose a tree
