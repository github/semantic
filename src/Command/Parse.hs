{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Command.Parse
(
-- Render parser trees
  jsonParseTree
, jsonIndexParseTree
, sExpressionParseTree
-- Parsers
, parserForLanguage
, parserForFilePath -- TODO: Remove. Langauge detection should not come from file extension.
-- Files and transcoding
, transcode
, sourceBlobsFromPaths
-- Git
, sourceBlobsFromSha
) where

import Category
import Data.Aeson (ToJSON, toJSON, encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Functor.Foldable hiding (Nil)
import Data.Record
import Data.String
import qualified Data.ByteString as B
import qualified Data.Text as T
import Git.Blob
import Git.Libgit2
import Git.Repository
import Git.Types
import qualified Git
import Info
import Language
import Language.Markdown
import Parser
import Prologue
import Source
import Syntax
import System.FilePath
import Term
import TreeSitter
import Renderer.JSON()
import Renderer.SExpression
import Text.Parser.TreeSitter.C
import Text.Parser.TreeSitter.Go
import Text.Parser.TreeSitter.JavaScript
import Text.Parser.TreeSitter.Ruby
import Text.Parser.TreeSitter.TypeScript
import System.IO
import Control.Exception (catch, IOException)
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect

-- TODO: This doesn't belong in here

-- | Return a parser for a given langauge or the lineByLineParser parser.
parserForLanguage :: Maybe Language -> Parser (Syntax Text) (Record DefaultFields)
parserForLanguage Nothing = lineByLineParser
parserForLanguage (Just language) = case language of
  C -> treeSitterParser C tree_sitter_c
  JavaScript -> treeSitterParser JavaScript tree_sitter_javascript
  TypeScript -> treeSitterParser TypeScript tree_sitter_typescript
  Markdown -> cmarkParser
  Ruby -> treeSitterParser Ruby tree_sitter_ruby
  Language.Go -> treeSitterParser Language.Go tree_sitter_go

-- | Return a parser based on the file extension (including the ".").
-- | TODO: Remove.
parserForType :: String -> Parser (Syntax Text) (Record DefaultFields)
parserForType = parserForLanguage . languageForType

-- | Return the parser that should be used for a given path.
-- | TODO: Remove.
parserForFilePath :: FilePath -> Parser (Syntax Text) (Record DefaultFields)
parserForFilePath = parserForType . toS . takeExtension

-- | A fallback parser that treats a file simply as rows of strings.
lineByLineParser :: Parser (Syntax Text) (Record DefaultFields)
lineByLineParser SourceBlob{..} = pure . cofree . root $ case foldl' annotateLeaves ([], 0) lines of
  (leaves, _) -> cofree <$> leaves
  where
    lines = actualLines source
    root children = (sourceRange :. Program :. rangeToSourceSpan source sourceRange :. Nil) :< Indexed children
    sourceRange = Source.totalRange source
    leaf charIndex line = (Range charIndex (charIndex + T.length line) :. Program :. rangeToSourceSpan source (Range charIndex (charIndex + T.length line)) :. Nil) :< Leaf line
    annotateLeaves (accum, charIndex) line =
      (accum <> [ leaf charIndex (Source.toText line) ] , charIndex + Source.length line)



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

-- | Constructs IndexFile nodes for the provided arguments and encodes them to JSON.
jsonIndexParseTree :: Bool -> [SourceBlob] -> IO ByteString
jsonIndexParseTree debug = fmap (toS . encode) . parseRoot debug IndexFile (\ node siblings -> node : concat siblings)

-- | Constructs ParseTreeFile nodes for the provided arguments and encodes them to JSON.
jsonParseTree :: Bool -> [SourceBlob] -> IO ByteString
jsonParseTree debug = fmap (toS . encode) . parseRoot debug ParseTreeFile Rose

-- | Determines the term decorator to use when parsing.
parseDecorator :: (Functor f, HasField fields Range) => Bool -> (Source -> TermDecorator f fields (Maybe SourceText))
parseDecorator True = termSourceTextDecorator
parseDecorator False = const . const Nothing

-- | For the given absolute file paths, retrieves their source blobs.
sourceBlobsFromPaths :: [FilePath] -> IO [SourceBlob]
sourceBlobsFromPaths filePaths =
  for filePaths (\filePath -> do
                  source <- readAndTranscodeFile filePath
                  pure $ Source.SourceBlob source mempty filePath (Just Source.defaultPlainBlob))

-- | For the given sha, git repo path, and file paths, retrieves the source blobs.
sourceBlobsFromSha :: String -> String -> [FilePath] -> IO [SourceBlob]
sourceBlobsFromSha commitSha gitDir filePaths = do
  maybeBlobs <- withRepository lgFactory gitDir $ do
    repo   <- getRepository
    object <- parseObjOid (toS commitSha)
    commit <- lookupCommit object
    tree   <- lookupTree (commitTree commit)
    lift $ runReaderT (traverse (toSourceBlob tree) filePaths) repo

  pure $ catMaybes maybeBlobs

  where
    toSourceBlob :: Git.Tree LgRepo -> FilePath -> ReaderT LgRepo IO (Maybe SourceBlob)
    toSourceBlob tree filePath = do
      entry <- treeEntry tree (toS filePath)
      case entry of
        Just (BlobEntry entryOid entryKind) -> do
          blob <- lookupBlob entryOid
          bytestring <- blobToByteString blob
          let oid = renderObjOid $ blobOid blob
          s <- liftIO $ transcode bytestring
          pure . Just $ SourceBlob s (toS oid) filePath (Just (toSourceKind entryKind))
        _ -> pure Nothing
      where
        toSourceKind :: Git.BlobKind -> SourceKind
        toSourceKind (Git.PlainBlob mode) = Source.PlainBlob mode
        toSourceKind (Git.ExecutableBlob mode) = Source.ExecutableBlob mode
        toSourceKind (Git.SymlinkBlob mode) = Source.SymlinkBlob mode

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

-- | Read the file and convert it to Unicode.
readAndTranscodeFile :: FilePath -> IO Source
readAndTranscodeFile path = do
  size <- fileSize path
  text <- case size of
    0 -> pure B.empty
    _ -> B.readFile path
  transcode text

-- Based on https://github.com/haskell/bytestring/pull/79/files
-- Neccessary to be able to handle /dev/null as a file.
fileSize :: FilePath -> IO Integer
fileSize f = withBinaryFile f ReadMode $ \h -> do
  -- hFileSize fails if file is not regular file (like /dev/null). Catch
  -- exception and return 0 in that case.
  filesz <- catch (hFileSize h) useZeroIfNotRegularFile
  pure $ fromIntegral filesz `max` 0
  where useZeroIfNotRegularFile :: IOException -> IO Integer
        useZeroIfNotRegularFile _ = pure 0

-- | Transcode a file to a unicode source.
transcode :: B.ByteString -> IO Source
transcode text = fromText <$> do
  match <- Detect.detectCharset text
  converter <- Convert.open match Nothing
  pure $ Convert.toUnicode converter text


data RoseF a b = RoseF a [b]
  deriving (Eq, Functor, Show)

type instance Base (Rose a) = RoseF a

instance Recursive (Rose a) where
  project (Rose a tree) = RoseF a tree

instance Corecursive (Rose a) where
  embed (RoseF a tree) = Rose a tree
