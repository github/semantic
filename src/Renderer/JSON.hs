{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Renderer.JSON
( json
, jsonParseTree
, ParseTreeFile(..)
) where

import Alignment
import Data.Aeson (ToJSON, toJSON, encode, object, (.=))
import Data.Aeson as A hiding (json)
import Data.Aeson.Types (Pair)
import Data.Bifunctor.Join
import Data.Functor.Both
import Data.Functor.Foldable hiding (Nil)
import Data.Record
import Data.These
import Data.Vector as Vector hiding (toList)
import Diff
import Info
import Parser
import Parser.Language
import Prologue
import qualified Data.Map as Map
import qualified Data.Text as T
import Source
import SplitDiff
import Syntax as S
import Term

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
termFields info syntax = "range" .= byteRange info : "category" .= Info.category info : syntaxToTermField syntax

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
data ParseTreeFile = ParseTreeFile { parseTreeFilePath :: FilePath, node :: Rose ParseNode } deriving (Show)

data Rose a = Rose a [Rose a]
  deriving (Eq, Show)

instance ToJSON ParseTreeFile where
  toJSON ParseTreeFile{..} = object [ "filePath" .= parseTreeFilePath, "programNode" .= cata algebra node ]
    where algebra (RoseF a as) = object $ parseNodeToJSONFields a <> [ "children" .= as ]


data IndexFile = IndexFile { indexFilePath :: FilePath, nodes :: [ParseNode] } deriving (Show)

instance ToJSON IndexFile where
  toJSON IndexFile{..} = object [ "filePath" .= indexFilePath, "programNodes" .= foldMap (singleton . object . parseNodeToJSONFields) nodes ]
    where singleton a = [a]

data ParseNode = ParseNode
  { category :: Text
  , sourceRange :: Range
  , sourceText :: Maybe SourceText
  , sourceSpan :: SourceSpan
  , identifier :: Maybe Text
  }
  deriving (Show)

-- | Produce a list of JSON 'Pair's for the fields in a given ParseNode.
parseNodeToJSONFields :: ParseNode -> [Pair]
parseNodeToJSONFields ParseNode{..} =
     [ "category" .= category, "sourceRange" .= sourceRange, "sourceSpan" .= sourceSpan ]
  <> [ "sourceText" .= sourceText | isJust sourceText ]
  <> [ "identifier" .= identifier | isJust identifier ]


jsonParseTree :: Bool -> SourceBlob -> ByteString
jsonParseTree = undefined

-- -- | Constructs ParseTreeFile nodes for the provided arguments and encodes them to JSON.
-- jsonParseTree :: Bool -> [SourceBlob] -> IO ByteString
-- jsonParseTree debug = fmap (toS . encode) . parseRoot debug ParseTreeFile Rose
--
-- -- | Constructs IndexFile nodes for the provided arguments and encodes them to JSON.
-- jsonIndexParseTree :: Bool -> [SourceBlob] -> IO ByteString
-- jsonIndexParseTree debug = fmap (toS . encode) . parseRoot debug IndexFile (\ node siblings -> node : Prologue.concat siblings)

parseRoot :: Bool -> (FilePath -> f ParseNode -> root) -> (ParseNode -> [f ParseNode] -> f ParseNode) -> [SourceBlob] -> IO [root]
parseRoot debug construct combine blobs = for blobs (\ sourceBlob@SourceBlob{..} -> do
  parsedTerm <- parseWithDecorator (decorator source) path sourceBlob
  pure $! construct path (para algebra parsedTerm))
  where algebra (annotation :< syntax) = combine (makeNode annotation (Prologue.fst <$> syntax)) (toList (Prologue.snd <$> syntax))
        decorator = parseDecorator debug
        makeNode :: Record (Maybe SourceText ': DefaultFields) -> Syntax Text (Term (Syntax Text) (Record (Maybe SourceText ': DefaultFields))) -> ParseNode
        makeNode (head :. range :. category :. sourceSpan :. Nil) syntax =
          ParseNode (toS category) range head sourceSpan (identifierFor syntax)

-- | Determines the term decorator to use when parsing.
parseDecorator :: (Functor f, HasField fields Range) => Bool -> (Source -> TermDecorator f fields (Maybe SourceText))
parseDecorator True = termSourceTextDecorator
parseDecorator False = const . const Nothing

-- | Returns a Just identifier text if the given Syntax term contains an identifier (leaf) syntax. Otherwise returns Nothing.
identifierFor :: (HasField fields (Maybe SourceText), HasField fields Category, StringConv leaf Text) => Syntax leaf (Term (Syntax leaf) (Record fields)) -> Maybe Text
identifierFor = fmap toS . extractLeafValue . unwrap <=< maybeIdentifier

-- | Return a parser incorporating the provided TermDecorator.
parseWithDecorator :: TermDecorator (Syntax Text) DefaultFields field -> FilePath -> Parser (Syntax Text) (Record (field ': DefaultFields))
parseWithDecorator decorator path blob = decorateTerm decorator <$> parserForFilePath path blob

-- | Decorate a 'Term' using a function to compute the annotation values at every node.
decorateTerm :: (Functor f) => TermDecorator f fields field -> Term f (Record fields) -> Term f (Record (field ': fields))
decorateTerm decorator = cata $ \ term -> cofree ((decorator term :. headF term) :< tailF term)

-- | A function computing a value to decorate terms with. This can be used to cache synthesized attributes on terms.
type TermDecorator f fields field = TermF f (Record fields) (Term f (Record (field ': fields))) -> field

-- | Term decorator extracting the source text for a term.
termSourceTextDecorator :: (Functor f, HasField fields Range) => Source -> TermDecorator f fields (Maybe SourceText)
termSourceTextDecorator source (ann :< _) = Just (SourceText (toText (Source.slice (byteRange ann) source)))

newtype Identifier = Identifier Text
  deriving (Eq, Show, ToJSON)

data RoseF a b = RoseF a [b]
  deriving (Eq, Functor, Show)

type instance Base (Rose a) = RoseF a

instance Recursive (Rose a) where
  project (Rose a tree) = RoseF a tree

instance Corecursive (Rose a) where
  embed (RoseF a tree) = Rose a tree
