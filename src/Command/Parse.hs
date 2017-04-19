{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Command.Parse
( jsonParseTree
, jsonIndexParseTree
, sExpressionParseTree
) where

import Category
import Data.Aeson (ToJSON, toJSON, encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Functor.Foldable hiding (Nil)
import Data.Record
import Info
import Parser
import Parser.Language
import Prologue
import Source
import Syntax
import Term
import Renderer.JSON()
import Renderer.SExpression


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


-- | Constructs ParseTreeFile nodes for the provided arguments and encodes them to JSON.
jsonParseTree :: Bool -> [SourceBlob] -> IO ByteString
jsonParseTree debug = fmap (toS . encode) . parseRoot debug ParseTreeFile Rose

-- | Constructs IndexFile nodes for the provided arguments and encodes them to JSON.
jsonIndexParseTree :: Bool -> [SourceBlob] -> IO ByteString
jsonIndexParseTree debug = fmap (toS . encode) . parseRoot debug IndexFile (\ node siblings -> node : concat siblings)

-- | Parses file contents into an SExpression format for the provided arguments.
sExpressionParseTree :: Bool -> [SourceBlob] -> IO ByteString
sExpressionParseTree _ blobs =
  pure . printTerms TreeOnly =<< parse blobs
  where parse = traverse (\sourceBlob@SourceBlob{..} -> parserForFilePath path sourceBlob)

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
