{-# LANGUAGE DataKinds, TypeOperators, DeriveAnyClass, ScopedTypeVariables #-}
module ParseCommand where

import Arguments
import Category
import Data.Aeson (ToJSON, toJSON, encode, object, (.=))
import Data.Record
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
import Renderer
import Renderer.JSON()
import Renderer.SExpression
import Text.Parser.TreeSitter.C
import Text.Parser.TreeSitter.Go
import Text.Parser.TreeSitter.JavaScript
import Text.Parser.TreeSitter.Ruby

data ParseJSON =
    ParseTreeProgramNode
    { category :: Text
    , sourceRange :: Range
    , sourceText :: Maybe SourceText
    , sourceSpan :: SourceSpan
    , identifier :: Maybe Text
    , children :: [ParseJSON]
    }
  | ParseTreeProgram
    { filePath :: FilePath
    , programNode :: ParseJSON
    }
  | IndexProgramNode
    { category :: Text
    , sourceRange :: Range
    , sourceText :: Maybe SourceText
    , sourceSpan :: SourceSpan
    , identifier :: Maybe Text
    }
  | IndexProgram
    { filePath :: FilePath
    , programNodes :: [ParseJSON]
    } deriving (Show, Generic)

instance ToJSON ParseJSON where
  toJSON ParseTreeProgramNode{..} = object
    $ [ "category" .= category, "sourceRange" .= sourceRange, "sourceSpan" .= sourceSpan, "children" .= children ]
    <> [ "sourceText" .= sourceText | isJust sourceText ]
    <> [ "identifier" .= identifier | isJust identifier ]
  toJSON ParseTreeProgram{..} = object [ "filePath" .= filePath, "programNode" .= programNode ]
  toJSON IndexProgramNode{..} = object
    $ [ "category" .= category, "sourceRange" .= sourceRange, "sourceSpan" .= sourceSpan]
    <> [ "sourceText" .= sourceText | isJust sourceText ]
    <> [ "identifier" .= identifier | isJust identifier ]
  toJSON IndexProgram{..} = object [ "filePath" .= filePath, "programNodes" .= programNodes ]


-- | Parses filePaths into two possible formats: SExpression or JSON.
parse :: Arguments -> IO ByteString
parse args@Arguments{..} =
  case format of
    SExpression -> renderSExpression args
    _ -> parse' args

  where
    renderSExpression :: Arguments -> IO ByteString
    renderSExpression args@Arguments{..} =
      case commitSha of
        Just commitSha' -> do
          sourceBlobs' <- sourceBlobs args (T.pack commitSha')
          terms' <- traverse (\sourceBlob@SourceBlob{..} -> conditionalParserWithSource debug path sourceBlob) sourceBlobs'
          return $ printTerms TreeOnly terms'
        Nothing -> do
          terms' <- sequenceA $ terms debug <$> filePaths
          return $ printTerms TreeOnly terms'


    parse' :: Arguments -> IO ByteString
    parse' args@Arguments{..} = fmap (toS . encode) (render filePaths)
      where
        render :: [FilePath] -> IO [ParseJSON]
        render filePaths =
          case commitSha of
            Just commitSha' -> do
              sourceBlobs' <- sourceBlobs args (T.pack commitSha')
              for sourceBlobs'
                (\sourceBlob@SourceBlob{..} -> do
                  terms' <- conditionalParserWithSource debug path sourceBlob
                  pure $ case format of
                            Index -> IndexProgram path (para parseIndexAlgebra terms')
                            _ -> ParseTreeProgram path (para parseTreeAlgebra terms'))

            Nothing -> traverse (constructParseNodes format) filePaths

    constructParseNodes :: Format -> FilePath -> IO ParseJSON
    constructParseNodes format filePath = do
      terms' <- terms debug filePath
      case format of
        Index -> return $ IndexProgram filePath (para parseIndexAlgebra terms')
        _ -> return $ ParseTreeProgram filePath (para parseTreeAlgebra terms')

    parseIndexAlgebra :: StringConv leaf T.Text => TermF (Syntax leaf) (Record '[(Maybe SourceText), Range, Category, SourceSpan]) (Term (Syntax leaf) (Record '[(Maybe SourceText), Range, Category, SourceSpan]), [ParseJSON]) -> [ParseJSON]
    parseIndexAlgebra (annotation :< syntax) = indexProgramNode annotation : (Prologue.snd =<< toList syntax)
      where indexProgramNode annotation = IndexProgramNode ((toS . Info.category) annotation) (byteRange annotation) (rhead annotation) (Info.sourceSpan annotation) (identifierFor (Prologue.fst <$> syntax))

    parseTreeAlgebra :: StringConv leaf T.Text => TermF (Syntax leaf) (Record '[(Maybe SourceText), Range, Category, SourceSpan]) (Term (Syntax leaf) (Record '[(Maybe SourceText), Range, Category, SourceSpan]), ParseJSON) -> ParseJSON
    parseTreeAlgebra (annotation :< syntax) = ParseTreeProgramNode ((toS . Info.category) annotation) (byteRange annotation) (rhead annotation) (Info.sourceSpan annotation) (identifierFor (Prologue.fst <$> syntax)) (Prologue.snd <$> toList syntax)

    identifierFor :: StringConv leaf T.Text => Syntax leaf (Term (Syntax leaf) (Record '[(Maybe SourceText), Range, Category, SourceSpan])) -> Maybe T.Text
    identifierFor = fmap toS . extractLeafValue . unwrap <=< maybeIdentifier

    -- | Returns syntax terms decorated with DefaultFields and SourceText. This is in IO because we read the file to extract the source text. SourceText is added to each term's annotation.
    terms :: Bool -> FilePath -> IO (SyntaxTerm Text '[(Maybe SourceText), Range, Category, SourceSpan])
    terms debug filePath = do
      source <- readAndTranscodeFile filePath
      parser filePath $ sourceBlob' filePath source

      where
        sourceBlob' :: FilePath -> Source -> SourceBlob
        sourceBlob' filePath source = Source.SourceBlob source mempty filePath (Just Source.defaultPlainBlob)

        parser :: FilePath -> Parser (Syntax Text) (Record '[(Maybe SourceText), Range, Category, SourceSpan])
        parser = conditionalParserWithSource debug

-- | For the file paths and commit sha provided, extract only the BlobEntries and represent them as SourceBlobs.
sourceBlobs :: Arguments -> Text -> IO [SourceBlob]
sourceBlobs Arguments{..} commitSha' = do
  maybeBlobs <- withRepository lgFactory gitDir $ do
    repo   <- getRepository
    object <- parseObjOid commitSha'
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

-- | Return a parser that decorates with the source text.
conditionalParserWithSource :: Bool -> FilePath -> Parser (Syntax Text) (Record '[(Maybe SourceText), Range, Category, SourceSpan])
conditionalParserWithSource debug path blob = decorateTerm (termSourceDecorator debug (source blob)) <$> parserForType (toS (takeExtension path)) blob

-- | Return a parser based on the file extension (including the ".").
parserForType :: Text -> Parser (Syntax Text) (Record '[Range, Category, SourceSpan])
parserForType mediaType = case languageForType mediaType of
  Just C -> treeSitterParser C tree_sitter_c
  Just JavaScript -> treeSitterParser JavaScript tree_sitter_javascript
  Just Markdown -> cmarkParser
  Just Ruby -> treeSitterParser Ruby tree_sitter_ruby
  Just Language.Go -> treeSitterParser Language.Go tree_sitter_go
  _ -> lineByLineParser

-- | Decorate a 'Term' using a function to compute the annotation values at every node.
decorateTerm :: (Functor f) => TermDecorator f fields field -> Term f (Record fields) -> Term f (Record (field ': fields))
decorateTerm decorator = cata $ \ term -> cofree ((decorator (extract <$> term) :. headF term) :< tailF term)

-- | A function computing a value to decorate terms with. This can be used to cache synthesized attributes on terms.
type TermDecorator f fields field = TermF f (Record fields) (Record (field ': fields)) -> field

-- | Term decorator extracting the source text for a term.
termSourceDecorator :: (HasField fields Range) => Bool -> Source -> TermDecorator f fields (Maybe SourceText)
termSourceDecorator debug source c = if debug then Just . SourceText . toText $ Source.slice range' source else Nothing
 where range' = byteRange $ headF c

-- | A fallback parser that treats a file simply as rows of strings.
lineByLineParser :: Parser (Syntax Text) (Record '[Range, Category, SourceSpan])
lineByLineParser SourceBlob{..} = pure . cofree . root $ case foldl' annotateLeaves ([], 0) lines of
  (leaves, _) -> cofree <$> leaves
  where
    lines = actualLines source
    root children = (sourceRange :. Program :. rangeToSourceSpan source sourceRange :. Nil) :< Indexed children
    sourceRange = Source.totalRange source
    leaf charIndex line = (Range charIndex (charIndex + T.length line) :. Program :. rangeToSourceSpan source (Range charIndex (charIndex + T.length line)) :. Nil) :< Leaf line
    annotateLeaves (accum, charIndex) line =
      (accum <> [ leaf charIndex (Source.toText line) ] , charIndex + Source.length line)

-- | Return the parser that should be used for a given path.
parserForFilepath :: FilePath -> Parser (Syntax Text) (Record '[Range, Category, SourceSpan])
parserForFilepath = parserForType . toS . takeExtension
