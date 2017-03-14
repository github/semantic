{-# LANGUAGE DataKinds, TypeOperators, DeriveAnyClass #-}
module ParseCommand where

import Arguments
import Category
import Data.Aeson (ToJSON, toJSON, encode, object, (.=))
import Data.Record
import qualified Data.Text as T
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
    , sourceText :: SourceText
    , sourceSpan :: SourceSpan
    , children :: [ParseJSON]
    }
  | ParseTreeProgram
    { filePath :: FilePath
    , programNode :: ParseJSON
    }
  | IndexProgramNode
    { category :: Text
    , sourceRange :: Range
    , sourceText :: SourceText
    , sourceSpan :: SourceSpan
    }
  | IndexProgram
    { filePath :: FilePath
    , programNodes :: [ParseJSON]
    } deriving (Show, Generic)

instance ToJSON ParseJSON where
  toJSON ParseTreeProgramNode{..} = object [ "category" .= category, "sourceRange" .= sourceRange, "sourceText" .= sourceText, "sourceSpan" .= sourceSpan, "children" .= children ]
  toJSON ParseTreeProgram{..} = object [ "filePath" .= filePath, "programNode" .= programNode ]
  toJSON IndexProgramNode{..} = object [ "category" .= category, "sourceRange" .= sourceRange, "sourceText" .= sourceText, "sourceSpan" .= sourceSpan ]
  toJSON IndexProgram{..} = object [ "filePath" .= filePath, "programNodes" .= programNodes ]

-- | Parses filePaths into two possible formats: SExpression or JSON.
parse :: Arguments -> IO ByteString
parse args@Arguments{..} = do
  case format of
    SExpression -> renderSExpression args
    Index -> renderIndex args
    _ -> renderParseTree args

  where
    renderSExpression :: Arguments -> IO ByteString
    renderSExpression Arguments{..} =
      case blobEntry of
        Just blob -> undefined
        Nothing -> do
          terms' <- sequenceA $ terms <$> filePaths
          return $ printTerms TreeOnly terms'

    -- | Constructs a ParseJSON suitable for indexing for each file path.
    renderIndex :: Arguments -> IO ByteString
    renderIndex Arguments{..} = fmap (toS . encode) (for filePaths render)
      where
        render :: FilePath -> IO ParseJSON
        render filePath =
          case blobEntry of
            Just blob -> undefined
            _ -> do
              programNodes <- constructIndexProgramNodes filePath
              return $ IndexProgram filePath programNodes

        constructIndexProgramNodes :: FilePath -> IO [ParseJSON]
        constructIndexProgramNodes filePath = do
          terms' <- terms filePath
          return $ cata algebra terms'

        algebra :: TermF (Syntax leaf) (Record '[SourceText, Range, Category, SourceSpan]) [ParseJSON] -> [ParseJSON]
        algebra (annotation :< syntax) = IndexProgramNode (category' annotation) (range' annotation) (text' annotation) (sourceSpan' annotation) : concat syntax

    -- | Constructs a ParseJSON honoring the nested tree structure for each file path.
    renderParseTree :: Arguments -> IO ByteString
    renderParseTree Arguments{..} = fmap (toS . encode) (for filePaths render)
      where
        render :: FilePath -> IO ParseJSON
        render filePath =
          case blobEntry of
            Just blob -> undefined
            Nothing -> do
              programNodes <- constructParseTreeProgramNodes filePath
              return $ ParseTreeProgram filePath programNodes

        constructParseTreeProgramNodes :: FilePath -> IO ParseJSON
        constructParseTreeProgramNodes filePath = do
          terms' <- terms filePath
          return $ cata algebra terms'

        algebra :: TermF (Syntax leaf) (Record '[SourceText, Range, Category, SourceSpan]) ParseJSON -> ParseJSON
        algebra (annotation :< syntax) = ParseTreeProgramNode (category' annotation) (range' annotation) (text' annotation) (sourceSpan' annotation) (toList syntax)


    category' :: Record '[SourceText, Range, Category, SourceSpan] -> Text
    category' = toS . Info.category

    range' :: Record '[SourceText, Range, Category, SourceSpan] -> Range
    range' = byteRange

    text' :: Record '[SourceText, Range, Category, SourceSpan] -> SourceText
    text' = Info.sourceText

    sourceSpan' :: Record '[SourceText, Range, Category, SourceSpan] -> SourceSpan
    sourceSpan' = Info.sourceSpan

    -- | Returns syntax terms decorated with DefaultFields and SourceText. This is in IO because we read the file to extract the source text. SourceText is added to each term's annotation.
    terms :: FilePath -> IO (SyntaxTerm Text '[SourceText, Range, Category, SourceSpan])
    terms filePath = do
      source <- readAndTranscodeFile filePath
      parser filePath $ sourceBlob' filePath source

      where
        sourceBlob' :: FilePath -> Source -> SourceBlob
        sourceBlob' filePath source = Source.SourceBlob source mempty filePath (Just Source.defaultPlainBlob)

        parser :: FilePath -> Parser (Syntax Text) (Record '[SourceText, Range, Category, SourceSpan])
        parser = parserWithSource

-- | Return a parser that decorates with the source text.
parserWithSource :: FilePath -> Parser (Syntax Text) (Record '[SourceText, Range, Category, SourceSpan])
parserWithSource path blob = decorateTerm (termSourceDecorator (source blob)) <$> parserForType (toS (takeExtension path)) blob

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
termSourceDecorator :: (HasField fields Range) => Source -> TermDecorator f fields SourceText
termSourceDecorator source c = SourceText . toText $ Source.slice range' source
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
